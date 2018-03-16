{-# LANGUAGE CPP, TypeOperators #-}
-- | A 'Frame' is a finite 'Int'-indexed collection of rows.
module Frames.Frame where
import Data.Foldable
#if __GLASGOW_HASKELL__ < 800
import Data.Monoid
#endif
import Data.Semigroup (Semigroup)
import qualified Data.Vector as V
import Data.Vinyl.TypeLevel
import Frames.Rec (Record)
import Frames.RecF (rappend)

-- | A 'Frame' is a finite collection of rows indexed by 'Int'.
data Frame r = Frame { frameLength :: !Int
                     , frameRow    :: Int -> r }

-- | A 'Frame' whose rows are 'Record' values.
type FrameRec rs = Frame (Record rs)

instance Functor Frame where
  fmap f (Frame len g) = Frame len (f . g)

-- | Build a 'Frame' from any 'Foldable'. This simply uses a boxed
-- 'V.Vector' to hold each row. If you have a collection of 'Record's,
-- consider using 'Frames.InCore.toFrame'.
boxedFrame :: Foldable f => f r -> Frame r
boxedFrame xs = Frame (V.length v) (v V.!)
  where v = V.fromList (toList xs)

instance Eq r => Eq (Frame r) where
  Frame l1 r1 == Frame l2 r2 =
    l1 == l2 && and (map (\i -> r1 i == r2 i) [0 .. l1 - 1])

-- | The 'Monoid' instance for 'Frame' provides a mechanism for
-- vertical concatenation of 'Frame's. That is, @f1 <> f2@ will return
-- a new 'Frame' with the rows of @f1@ followed by the rows of @f2@.
instance Monoid (Frame r) where
  mempty = Frame 0 (const $ error "index out of bounds (empty frame)")
  Frame l1 f1 `mappend` Frame l2 f2 = Frame (l1+l2) $ \i ->
                                      if i < l1 then f1 i else f2 (i - l1)

instance Semigroup (Frame r) where

instance Foldable Frame where
  foldMap f (Frame n row) = foldMap (f . row) [0..n-1]
  {-# INLINE foldMap #-}
  foldl' f z (Frame n row) = foldl' ((. row) . f) z [0..n-1]
  {-# INLINE foldl' #-}

instance Applicative Frame where
  -- | A frame of 'maxBound' rows, each of which is the given value.
  pure x = Frame maxBound (const x)
  -- | Zips two 'Frame's together, applying the rows of the first to
  -- those of the second. The result has as many rows as the smaller
  -- of the two argument 'Frame's.
  Frame l1 f1 <*> Frame l2 f2 = Frame (min l1 l2) $ ($) <$> f1 <*> f2

instance Monad Frame where
  -- | A frame of 'maxBound' rows, each of which is the given value.
  return = pure
  -- | Like 'concatMap' for lists.
  Frame l f >>= fb = foldMap (fb . f) [0 .. l - 1]

-- | Horizontal 'Frame' concatenation. That is, @zipFrames f1 f2@ will
-- return a 'Frame' with as many rows as the smaller of @f1@ and @f2@
-- whose rows are the result of appending the columns of @f2@ to those
-- of @f1@.
zipFrames :: FrameRec rs -> FrameRec rs' -> FrameRec (rs ++ rs')
zipFrames (Frame l1 f1) (Frame l2 f2) =
    Frame (min l1 l2) $ rappend <$> f1 <*> f2
