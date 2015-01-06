{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module TutorialUsers where
import Control.Monad (mzero)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import Frames.CSV (ColumnTypeable(..))

data GenderT = Male | Female deriving (Eq,Ord,Show)

instance Readable GenderT where
  fromText t
      | t' == "m" = return Male
      | t' == "f" = return Female
      | otherwise = mzero
    where t' = T.toCaseFold t

data UserCol = TInt | TGender | TText deriving (Eq,Show,Ord,Enum,Bounded)

instance Monoid UserCol where
  mempty = maxBound
  mappend x y = toEnum $ max (fromEnum x) (fromEnum y)

instance ColumnTypeable UserCol where
  colType TInt = [t|Int|]
  colType TGender = [t|GenderT|]
  colType TText = [t|T.Text|]
  inferType = let isInt = fmap (const TInt :: Int -> UserCol) . fromText
                  isGen = bool Nothing (Just TGender) . (`elem` ["M","F"])
              in \t -> fromMaybe TText $ isInt t <> isGen t
