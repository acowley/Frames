{-# LANGUAGE DataKinds, DeriveDataTypeable, TypeFamilies, TypeOperators #-}
module TutorialZipCode where
import Control.Monad (mzero)
import qualified Data.Char as C
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Vector as V
import Frames.InCore (VectorFor)
import Frames

data ZipT = ZipUS Int Int Int Int Int
          | ZipWorld Char Char Char Char Char
  deriving (Eq, Ord, Show, Typeable)

type instance VectorFor ZipT = V.Vector

instance Readable ZipT  where
  fromText t
      | T.length t == 5 = let cs@[v,w,x,y,z] = T.unpack t
                              [a,b,c,d,e] = map C.digitToInt cs
                          in if all C.isDigit cs
                             then return $ ZipUS a b c d e
                             else return $ ZipWorld v w x y z
      | otherwise = mzero

type MyColumns = ZipT ': CommonColumns
