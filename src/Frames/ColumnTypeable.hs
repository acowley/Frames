{-# LANGUAGE BangPatterns, LambdaCase #-}
module Frames.ColumnTypeable where
import Language.Haskell.TH
import qualified Data.Text as T

-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> Q Type
  inferType :: T.Text -> a
