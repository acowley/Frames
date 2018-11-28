{-# LANGUAGE BangPatterns, CPP, DataKinds, EmptyCase,
             FlexibleInstances, PolyKinds, ScopedTypeVariables,
             TupleSections, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
-- | Efficient in-memory (in-core) storage of tabular data.
module Frames.InCore where
import Control.Monad.Primitive
import Control.Monad.ST (runST)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Vinyl (Rec(..))
import qualified Data.Vinyl as V
import Data.Vinyl.Functor (Compose(..), getCompose, ElField(..), (:.))
import Frames.Col
import Frames.Frame
import Frames.Rec
import Frames.RecF
#if __GLASGOW_HASKELL__ < 800
import GHC.Prim (RealWorld)
#endif
import GHC.TypeLits (KnownSymbol)
import GHC.Types (Symbol, Type)
import qualified Pipes as P
import qualified Pipes.Prelude as P

-- | The most efficient vector type for each column data type.
type family VectorFor t :: * -> *
type instance VectorFor Bool = VU.Vector
type instance VectorFor Int = VU.Vector
type instance VectorFor Float = VU.Vector
type instance VectorFor Double = VU.Vector
type instance VectorFor String = VB.Vector
type instance VectorFor Text = VB.Vector

-- | The mutable version of 'VectorFor' a particular type.
type VectorMFor a = VG.Mutable (VectorFor a)

-- | Since we stream into the in-memory representation, we use an
-- exponential growth strategy to resize arrays as more data is read
-- in. This is the initial capacity of each column.
initialCapacity :: Int
initialCapacity = 128

-- | Mutable vector types for each column in a row.
type family VectorMs m rs where
  VectorMs m '[] = '[]
  VectorMs m (s :-> a ': rs) =
    s :-> VectorMFor a (PrimState m) a ': VectorMs m rs

-- | Immutable vector types for each column in a row.
type family Vectors rs where
  Vectors '[] = '[]
  Vectors (s :-> a ': rs) = s :-> VectorFor a a ': Vectors rs

-- | Tooling to allocate, grow, write to, freeze, and index into
-- records of vectors.
class RecVec (rs :: [(Symbol,Type)]) where
  allocRec   :: PrimMonad m
             => proxy rs -> Int -> m (Record (VectorMs m rs))
  freezeRec  :: PrimMonad m
             => proxy rs -> Int -> Record (VectorMs m rs)
             -> m (Record (Vectors rs))
  growRec    :: PrimMonad m
             => proxy rs -> Record (VectorMs m rs) -> m (Record (VectorMs m rs))
  writeRec   :: PrimMonad m
             => proxy rs -> Int -> Record (VectorMs m rs) -> Record rs -> m ()
  indexRec   :: proxy rs -> Int -> Record (Vectors rs) -> Record rs
  produceRec :: proxy rs -> Record (Vectors rs) -> V.Rec (((->) Int) :. ElField) rs

-- The use of the type families Vectors and VectorMs interferes with
-- GHC's pattern match exhaustiveness checker, so we write down dummy
-- cases to avoid warnings. This is probably a GHC bug as writing the
-- apparently missing pattern match causes a type checker error!

instance RecVec '[] where
  allocRec _ _ = return V.RNil
  {-# INLINE allocRec #-}

  freezeRec _ _ V.RNil = return V.RNil
#if __GLASGOW_HASKELL__ < 800
  freezeRec _ _ x = case x of
#endif
  {-# INLINE freezeRec #-}

  growRec _ V.RNil = return V.RNil
#if __GLASGOW_HASKELL__ < 800
  growRec _ x = case x of
#endif
  {-# INLINE growRec #-}

  indexRec _ _ _ = V.RNil
  {-# INLINE indexRec #-}

  writeRec _ _ V.RNil V.RNil = return ()
#if __GLASGOW_HASKELL__ < 800
  writeRec _ _ x _ = case x of
#endif
  {-# INLINE writeRec #-}

  produceRec _ V.RNil = V.RNil
#if __GLASGOW_HASKELL__ < 800
  produceRec _ x = case x of
#endif
  {-# INLINE produceRec #-}

instance forall s a rs.
  (VGM.MVector (VectorMFor a) a,
   VG.Vector (VectorFor a) a,
   KnownSymbol s, RecVec rs)
  => RecVec (s :-> a ': rs) where
  allocRec _ size = (&:) <$> VGM.new size <*> allocRec (Proxy::Proxy rs) size
  {-# INLINE allocRec #-}

  freezeRec _ n (Col x :& xs) =
    (&:) <$> (VG.unsafeFreeze $ VGM.unsafeSlice 0 n x)
         <*> freezeRec (Proxy::Proxy rs) n xs
  freezeRec _ _ _ = error "Impossible case freezeRec"
  {-# INLINE freezeRec #-}

  growRec _ (Col x :& xs) = (&:) <$> VGM.grow x (VGM.length x)
                                              <*> growRec (Proxy :: Proxy rs) xs
  growRec _ _ = error "Impossible case growRec"
  {-# INLINE growRec #-}

  writeRec _ !i !(Col v :& vs) (Col x :& xs) =
    VGM.unsafeWrite v i x >> writeRec (Proxy::Proxy rs) i vs xs
  writeRec _ _ _ _ = error "Impossible case writeRec"
  {-# INLINE writeRec #-}

  indexRec _ !i !(Col x :& xs) =
    x VG.! i &: indexRec (Proxy :: Proxy rs) i xs
  indexRec _ _ _ = error "Impossible case indexRec"
  {-# INLINE indexRec #-}

  produceRec _ (Col v V.:& vs) = Compose (Field . (v VG.!)) :& produceRec (Proxy::Proxy rs) vs
  produceRec _ _ = error "Impossible case produceRec"
  {-# INLINE produceRec #-}

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generators a matter of indexing into a densely packed
-- representation. Returns the number of rows and a record of column
-- indexing functions. See 'toAoS' to convert the result to a 'Frame'
-- which provides an easier-to-use function that indexes into the
-- table in a row-major fashion.
inCoreSoA :: forall m rs. (PrimMonad m, RecVec rs)
          => P.Producer (Record rs) m ()
          -> m (Int, V.Rec (((->) Int) :. ElField) rs)
inCoreSoA xs =
  do mvs <- allocRec (Proxy :: Proxy rs) initialCapacity
     let feed (!i, !sz, !mvs') row
           | i == sz = growRec (Proxy::Proxy rs) mvs'
                       >>= flip feed row . (i, sz*2,)
           | otherwise = do writeRec (Proxy::Proxy rs) i mvs' row
                            return (i+1, sz, mvs')
         fin (n,_,mvs') =
           do vs <- freezeRec (Proxy::Proxy rs) n mvs'
              return . (n,) $ produceRec (Proxy::Proxy rs) vs
     P.foldM feed (return (0,initialCapacity,mvs)) fin xs
{-# INLINE inCoreSoA #-}

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generators a matter of indexing into a densely packed
-- representation. Returns a 'Frame' that provides a function to index
-- into the table.
inCoreAoS :: (PrimMonad m, RecVec rs)
          => P.Producer (Record rs) m () -> m (FrameRec rs)
inCoreAoS = fmap (uncurry toAoS) . inCoreSoA

-- | Like 'inCoreAoS', but applies the provided function to the record
-- of columns before building the 'Frame'.
inCoreAoS' :: (PrimMonad m, RecVec rs)
           => (V.Rec ((->) Int :. ElField) rs -> V.Rec ((->) Int :. ElField) ss)
           -> P.Producer (Record rs) m () -> m (FrameRec ss)
inCoreAoS' f = fmap (uncurry toAoS . aux) . inCoreSoA
  where aux (x,y) = (x, f y)

-- | Convert a structure-of-arrays to an array-of-structures. This can
-- simplify usage of an in-memory representation.
toAoS :: Int -> V.Rec ((->) Int :. ElField) rs -> FrameRec rs
toAoS n = Frame n . rtraverse getCompose
{-# INLINE toAoS #-}

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generator a matter of indexing into a densely packed
-- representation.
inCore :: forall m n rs. (PrimMonad m, RecVec rs, Monad n)
       => P.Producer (Record rs) m () -> m (P.Producer (Record rs) n ())
inCore xs =
  do mvs <- allocRec (Proxy :: Proxy rs) initialCapacity
     let feed (!i,!sz,!mvs') row
              | i == sz = growRec (Proxy::Proxy rs) mvs'
                          >>= flip feed row . (i, sz*2,)
              | otherwise = do writeRec (Proxy::Proxy rs) i mvs' row
                               return (i+1, sz, mvs')
         fin (n,_,mvs') =
           do vs <- freezeRec (Proxy::Proxy rs) n mvs'
              let spool !i
                    | i == n = pure ()
                    | otherwise = P.yield (indexRec Proxy i vs) >> spool (i+1)
              return $ spool 0
     P.foldM feed (return (0,initialCapacity,mvs)) fin xs
{-# INLINE inCore #-}

-- | Build a 'Frame' from a collection of 'Record's using efficient
-- column-based storage.
toFrame :: (P.Foldable f, RecVec rs) => f (Record rs) -> Frame (Record rs)
toFrame xs = runST $ inCoreAoS (P.each xs)
{-# INLINE toFrame #-}

-- | Keep only those rows of a 'FrameRec' that satisfy a predicate.
filterFrame :: RecVec rs => (Record rs -> Bool) -> FrameRec rs -> FrameRec rs
filterFrame p f = runST $ inCoreAoS $ P.each f P.>-> P.filter p
{-# INLINE filterFrame #-}

-- | Process a stream of 'Record's into a stream of 'Frame's that each
-- contains no more than the given number of records.
produceFrameChunks :: forall rs m. (RecVec rs, PrimMonad m)
                   => Int
                   -> P.Producer (Record rs) m ()
                   -> P.Producer (FrameRec rs) m ()
produceFrameChunks chunkSize = go
  where go src = do mutVecs <- P.lift (allocRec (Proxy :: Proxy rs) chunkSize)
                    goChunk src mutVecs 0
        goChunk src mutVecs !i
          | i >= chunkSize =
              do chunk <- P.lift (freezeFrame i mutVecs)
                 P.yield chunk
                 go src
          | otherwise =
            do maybeRow <- P.lift (P.next src)
               case maybeRow of
                 Left _ -> do
                   P.lift (freezeFrame i mutVecs) >>= P.yield
                 Right (r,src') -> do
                   P.lift (writeRec (Proxy::Proxy rs) i mutVecs r)
                   goChunk src' mutVecs (i+1)
        freezeFrame :: Int -> Record (VectorMs m rs) -> m (FrameRec rs)
        freezeFrame n =
          fmap (toAoS n . produceRec (Proxy::Proxy rs))
          . freezeRec (Proxy::Proxy rs) n
{-# INLINABLE produceFrameChunks #-}

-- | Split a 'Frame' into chunks of no more than the given number of
-- records. The underlying memory is shared with the original 'Frame'.
frameChunks :: Int -> FrameRec rs -> [FrameRec rs]
frameChunks chunkSize whole = map aux [ 0, chunkSize .. frameLength whole - 1 ]
  where aux i = Frame (min (frameLength whole - i) chunkSize)
                      (frameRow whole . (+ i))
{-# INLINABLE frameChunks #-}
