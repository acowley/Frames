{-# LANGUAGE CPP, OverloadedStrings #-}
module Frames.Utils (capitalize1, sanitizeTypeName) where

import Control.Arrow (first)
import Data.Char (isAlpha, isAlphaNum, toUpper)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif
import qualified Data.Text as T

-- | Capitalize the first letter of a 'T.Text'.
capitalize1 :: T.Text -> T.Text
capitalize1 = foldMap (onHead toUpper) . T.split (not . isAlphaNum)
  where onHead f = maybe mempty (uncurry T.cons . first f) . T.uncons

-- | Massage a column name from a CSV file into a valid Haskell type
-- identifier.
sanitizeTypeName :: T.Text -> T.Text
sanitizeTypeName = unreserved . fixupStart
                 . T.concat . T.split (not . valid) . capitalize1
  where valid c = isAlphaNum c || c == '\'' || c == '_'
        unreserved t
          | t `elem` ["Type", "Class"] = "Col" <> t
          | otherwise = t
        fixupStart t = case T.uncons t of
                         Nothing -> "Col"
                         Just (c,_) | isAlpha c -> t
                                    | otherwise -> "Col" <> t
