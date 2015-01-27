{-# LANGUAGE BangPatterns,
             DataKinds,
             EmptyCase,
             FlexibleInstances,
             ScopedTypeVariables,
             TupleSections,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances #-}
-- | Efficient in-memory (in-core) storage of tabular data.
module Frames.InCore where
import Control.Applicative
import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy
import Data.Text (Text)
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vinyl as V
import Data.Vinyl.Functor (Identity(..))
import Frames.Col
import Frames.Frame
import Frames.Rec
import Frames.RecF
import GHC.Prim (RealWorld)
import qualified Pipes as P
import qualified Pipes.Prelude as P

-- | The most efficient vector type for each column data type.
type family VectorFor t :: * -> *
type instance VectorFor Bool = VU.Vector
type instance VectorFor Int = VU.Vector
type instance VectorFor Double = VU.Vector
type instance VectorFor Text = VB.Vector

-- | The mutable version of 'VectorFor' a particular type.
type VectorMFor a = VG.Mutable (VectorFor a)

-- | Since we stream into the in-memory representation, we use an
-- exponential growth strategy to resize arrays as more data is read
-- in. This is the initial capacity of each column.
initialCapacity :: Int
initialCapacity = 128

-- | Mutable vector types for each column in a row.
type family VectorMs rs where
  VectorMs '[] = '[]
  VectorMs (s :-> a ': rs) = s :-> VectorMFor a RealWorld a ': VectorMs rs

-- | Immutable vector types for each column in a row.
type family Vectors rs where
  Vectors '[] = '[]
  Vectors (s :-> a ': rs) = s :-> VectorFor a a ': Vectors rs

-- | Tooling to allocate, grow, write to, freeze, and index into
-- records of vectors.
class RecVec rs where
  allocRec :: proxy rs -> IO (Rec (VectorMs rs))
  freezeRec :: proxy rs -> Int -> Rec (VectorMs rs) -> IO (Rec (Vectors rs))
  growRec :: proxy rs -> Rec (VectorMs rs) -> IO (Rec (VectorMs rs))
  writeRec :: proxy rs -> Int -> Rec (VectorMs rs) -> Rec rs -> IO ()
  indexRec :: proxy rs -> Int -> Rec (Vectors rs) -> Rec rs
  produceRec :: proxy rs -> Rec (Vectors rs) -> RecF ((->) Int) rs

-- The use of the type families Vectors and VectorMs interferes with
-- GHC's pattern match exhaustiveness checker, so we write down dummy
-- cases to avoid warnings. This is probably a GHC bug as writing the
-- apparently missing pattern match causes a type checker error!

instance RecVec '[] where
  allocRec _ = return Nil
  {-# INLINE allocRec #-}

  freezeRec _ _ V.RNil = return V.RNil
  freezeRec _ _ x = case x of
  {-# INLINE freezeRec #-}

  growRec _ V.RNil = return V.RNil
  growRec _ x = case x of
  {-# INLINE growRec #-}

  indexRec _ _ _ = V.RNil
  {-# INLINE indexRec #-}

  writeRec _ _ V.RNil V.RNil = return ()
  writeRec _ _ x _ = case x of
  {-# INLINE writeRec #-}

  produceRec _ V.RNil = V.RNil
  produceRec _ x = case x of
  {-# INLINE produceRec #-}

instance forall s a rs.
  (VGM.MVector (VectorMFor a) a,
   VG.Mutable (VectorFor a) ~ VectorMFor a,
   VG.Vector (VectorFor a) a,
   RecVec rs)
  => RecVec (s :-> a ': rs) where
  allocRec _ = (&:) <$> VGM.new initialCapacity <*> allocRec (Proxy::Proxy rs)
  {-# INLINE allocRec #-}

  freezeRec _ n (Identity (Col x) V.:& xs) =
    (&:) <$> (VG.unsafeFreeze $ VGM.unsafeSlice 0 n x)
         <*> freezeRec (Proxy::Proxy rs) n xs
  freezeRec _ _ x = case x of
  {-# INLINE freezeRec #-}

  growRec _ (Identity (Col x) V.:& xs) = (&:) <$> VGM.grow x (VGM.length x)
                                              <*> growRec (Proxy :: Proxy rs) xs
  growRec _ x = case x of
  {-# INLINE growRec #-}

  writeRec _ !i !(Identity (Col v) V.:& vs) (Identity (Col x) V.:& xs) =
    VGM.unsafeWrite v i x >> writeRec (Proxy::Proxy rs) i vs xs
  writeRec _ _ _ x = case x of
  {-# INLINE writeRec #-}

  indexRec _ !i !(Identity (Col x) V.:& xs) =
    x VG.! i &: indexRec (Proxy :: Proxy rs) i xs
  indexRec _ _ x = case x of
  {-# INLINE indexRec #-}

  produceRec _ (Identity (Col v) V.:& vs) = frameCons (v VG.!) $
                                            produceRec (Proxy::Proxy rs) vs
  produceRec _ x = case x of
  {-# INLINE produceRec #-}

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generators a matter of indexing into a densely packed
-- representation. Returns the number of rows and a record of column
-- indexing functions. See 'toAoS' to convert the result to a 'Frame'
-- which provides an easier-to-use function that indexes into the
-- table in a row-major fashion.
inCoreSoA :: forall m rs. (MonadIO m, RecVec rs)
          => P.Producer (Rec rs) m () -> m (Int, RecF ((->) Int) rs)
inCoreSoA xs =
  do mvs <- liftIO $ allocRec (Proxy :: Proxy rs)
     let feed (!i, !sz, !mvs') row
           | i == sz = liftIO (growRec (Proxy::Proxy rs) mvs')
                       >>= flip feed row . (i, sz*2,)
           | otherwise = do liftIO $ writeRec (Proxy::Proxy rs) i mvs' row
                            return (i+1, sz, mvs')
         fin (n,_,mvs') =
           do vs <- liftIO $ freezeRec (Proxy::Proxy rs) n mvs'
              return . (n,) $ produceRec (Proxy::Proxy rs) vs
     P.foldM feed (return (0,initialCapacity,mvs)) fin xs
{-# INLINE inCoreSoA #-}

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generators a matter of indexing into a densely packed
-- representation. Returns a 'Frame' that provides a function to index
-- into the table.
inCoreAoS :: (Functor m, MonadIO m, RecVec rs)
          => P.Producer (Rec rs) m () -> m (FrameRec rs)
inCoreAoS = fmap (uncurry toAoS) . inCoreSoA

-- | Like 'inCoreAoS', but applies the provided function to the record
-- of columns before building the 'Frame'.
inCoreAoS' :: (Functor m, MonadIO m, RecVec rs)
           => (RecF ((->) Int) rs -> RecF ((->) Int) ss)
           -> P.Producer (Rec rs) m () -> m (FrameRec ss)
inCoreAoS' f = fmap (uncurry toAoS . aux) . inCoreSoA
  where aux (x,y) = (x, f y)

-- | Convert a structure-of-arrays to an array-of-structures. This can
-- simplify usage of an in-memory representation.
toAoS :: Int -> RecF ((->) Int) rs -> FrameRec rs
toAoS n = Frame n . rtraverse (fmap Identity)
{-# INLINE toAoS #-}

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generator a matter of indexing into a densely packed
-- representation.
inCore :: forall m n rs. (Functor m, MonadIO m, RecVec rs, Monad n)
       => P.Producer (Rec rs) m () -> m (P.Producer (Rec rs) n ())
inCore xs =
  do mvs <- liftIO $ allocRec (Proxy :: Proxy rs)
     let feed (!i,!sz,!mvs') row
              | i == sz = liftIO (growRec (Proxy::Proxy rs) mvs')
                          >>= flip feed row . (i, sz*2,)
              | otherwise = do liftIO $ writeRec (Proxy::Proxy rs) i mvs' row
                               return (i+1, sz, mvs')
         fin (n,_,mvs') =
           do vs <- liftIO $ freezeRec (Proxy::Proxy rs) n mvs'
              let spool !i
                    | i == n = pure ()
                    | otherwise = P.yield (indexRec Proxy i vs) >> spool (i+1)
              return $ spool 0
     P.foldM feed (return (0,initialCapacity,mvs)) fin xs
{-# INLINE inCore #-}
