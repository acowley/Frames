{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module TutorialUsers where
import Control.Monad (liftM, mzero)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import Data.Traversable (sequenceA)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Frames.CSV (ColumnTypeable(..))
import Frames.InCore (VectorFor)

data GenderT = Male | Female deriving (Enum,Eq,Ord,Show)

instance Readable GenderT where
  fromText t
      | t' == "m" = return Male
      | t' == "f" = return Female
      | otherwise = mzero
    where t' = T.toCaseFold t

data UserCol = TInt | TGender | TText deriving (Eq,Show,Ord,Enum,Bounded)

instance Monoid UserCol where
  mempty = maxBound
  mappend x y = if x == y then x else TText

instance ColumnTypeable UserCol where
  colType TInt = [t|Int|]
  colType TGender = [t|GenderT|]
  colType TText = [t|T.Text|]
  inferType = let isInt = fmap (const TInt :: Int -> UserCol) . fromText
                  isGen = bool Nothing (Just TGender) . (`elem` ["M","F"])
              in fromMaybe TText . mconcat . sequenceA [isGen, isInt]

type instance VectorFor GenderT = V.Vector

-- * Packed Representation

newtype GenderU = GenderU { getGenderU :: GenderT } deriving (Eq,Ord,Show,Enum)

-- Let's go all the way and support an efficient packed representation
-- for our type. If you don't want to, or can't, provide an 'VU.Unbox'
-- or 'Storable' instance for your type, define a 'VectorFor' type
-- instance that uses a boxed vector for your type.
newtype instance VU.MVector s GenderU = MV_Gender (VU.MVector s Bool)
newtype instance VU.Vector GenderU = V_Gender (VU.Vector Bool)

instance VGM.MVector VU.MVector GenderU where
  {-# INLINE basicLength #-}
  basicLength (MV_Gender v) = VGM.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice start len (MV_Gender v) = MV_Gender $
                                             VGM.basicUnsafeSlice start len v
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Gender v) (MV_Gender w) = VGM.basicOverlaps v w
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = liftM MV_Gender (VGM.basicUnsafeNew n)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Gender v) = liftM (toEnum . fromEnum)
                                . VGM.basicUnsafeRead v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Gender v) i x = VGM.basicUnsafeWrite v i
                                     $ toEnum (fromEnum x)

instance VG.Vector VU.Vector GenderU where
  {-# INLINE basicLength #-}
  basicLength (V_Gender v) = VG.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice start len (V_Gender v) = V_Gender $
                                            VG.basicUnsafeSlice start len v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Gender v) = liftM V_Gender $ VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Gender v) = liftM MV_Gender $ VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Gender v) = liftM (toEnum . fromEnum) . VG.basicUnsafeIndexM v

instance VU.Unbox GenderU where

type instance VectorFor GenderU = VU.Vector
