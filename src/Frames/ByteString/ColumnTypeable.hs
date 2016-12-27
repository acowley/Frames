{-# LANGUAGE BangPatterns, DefaultSignatures, LambdaCase #-}
module Frames.ByteString.ColumnTypeable where
import Control.Monad (MonadPlus)
import Data.Readable (Readable(fromBS))
import qualified Data.Text as T
import Language.Haskell.TH
import qualified Data.ByteString.Char8 as C8

data Parsed a = Possibly a | Definitely a deriving (Eq, Ord, Show)

instance Functor Parsed where
  fmap f (Possibly x) = Possibly (f x)
  fmap f (Definitely x) = Definitely (f x)

-- | Values that can be read from a 'C8.ByteString' with more or less
-- discrimination.
class Parseable a where
  -- | Returns 'Nothing' if a value of the given type can not be read;
  -- returns 'Just Possibly' if a value can be read, but is likely
  -- ambiguous (e.g. an empty string); returns 'Just Definitely' if a
  -- value can be read and is unlikely to be ambiguous."
  parse :: MonadPlus m => C8.ByteString -> m (Parsed a)
  default parse :: (Readable a, MonadPlus m)
                => C8.ByteString -> m (Parsed a)
  parse = fmap Definitely . fromBS
  {-# INLINE parse #-}

-- | Discard any estimate of a parse's ambiguity.
discardConfidence :: Parsed a -> a
discardConfidence (Possibly x) = x
discardConfidence (Definitely x) = x

-- | Acts just like 'fromText': tries to parse a value from a 'C8.ByteString'
-- and discards any estimate of the parse's ambiguity.
parse' :: (MonadPlus m, Parseable a) => C8.ByteString -> m a
parse' = fmap discardConfidence . parse

instance Parseable Bool where
instance Parseable Int where
instance Parseable Float where
instance Parseable Double where
  -- Some CSV's export Doubles in a format like '1,000.00', filtering out commas lets us parse those sucessfully
  parse = fmap Definitely . fromBS . C8.filter (/= ',')
instance Parseable C8.ByteString where

-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> Q Type
  inferType :: C8.ByteString -> a
