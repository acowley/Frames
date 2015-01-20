{-# LANGUAGE DataKinds, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, OverloadedStrings, TemplateHaskell,
             TypeFamilies, TypeOperators #-}
module TutorialUsers where
import Control.Monad (liftM, mzero)
import Data.Readable (Readable(fromText))
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Frames.InCore (VectorFor)
import Frames

data GenderT = Male | Female deriving (Enum,Eq,Ord,Show,Typeable)

type instance VectorFor GenderT = V.Vector

instance Readable GenderT where
  fromText "M" = return Male
  fromText "F" = return Female
  fromText _ = mzero

type MyColumns = GenderT ': CommonColumns

-- * Packed Representation

newtype GenderU = GenderU { getGenderU :: GenderT }
    deriving (Eq,Ord,Show,Enum,Typeable)

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
