{-# LANGUAGE BangPatterns, DefaultSignatures, LambdaCase #-}
module Frames.ColumnTypeable where
import Control.Monad (MonadPlus)
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import Language.Haskell.TH

data Parsed a = Possibly a | Definitely a deriving (Eq, Ord, Show)

instance Functor Parsed where
  fmap f (Possibly x) = Possibly (f x)
  fmap f (Definitely x) = Definitely (f x)

-- | Values that can be read from a 'T.Text' with more or less
-- discrimination.
class Parseable a where
  -- | Returns 'Nothing' if a value of the given type can not be read;
  -- returns 'Just Possibly' if a value can be read, but is likely
  -- ambiguous (e.g. an empty string); returns 'Just Definitely' if a
  -- value can be read and is unlikely to be ambiguous."
  parse :: (Functor m, MonadPlus m) => T.Text -> m (Parsed a)
  default parse :: (Readable a, Functor m, MonadPlus m)
                => T.Text -> m (Parsed a)
  parse = fmap Definitely . fromText
  {-# INLINE parse #-}

-- | Discard any estimate of a parse's ambiguity.
discardConfidence :: Parsed a -> a
discardConfidence (Possibly x) = x
discardConfidence (Definitely x) = x

-- | Acts just like 'fromText': tries to parse a value from a 'T.Text'
-- and discards any estimate of the parse's ambiguity.
parse' :: (Functor m, MonadPlus m, Parseable a) => T.Text -> m a
parse' = fmap discardConfidence . parse

instance Parseable Bool where
instance Parseable Int where
instance Parseable Float where
instance Parseable Double where
instance Parseable T.Text where

-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> Q Type
  inferType :: T.Text -> a
