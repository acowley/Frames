{-# LANGUAGE DefaultSignatures #-}
module Frames.ShowCSV where
import Data.Text (Text)
import qualified Data.Text as T

-- | Control the way values of a type are printed when serializing to
-- a CSV stream.
class ShowCSV a where
  showCSV :: a -> Text
  default showCSV :: Show a => a -> Text
  showCSV = T.pack . show

instance ShowCSV Bool where
instance ShowCSV Int where
instance ShowCSV Double where
instance ShowCSV Text where
  showCSV = id
