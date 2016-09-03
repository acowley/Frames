{-# LANGUAGE BangPatterns, DefaultSignatures, LambdaCase #-}
module Frames.ColumnTypeable where
import Control.Monad (MonadPlus, mzero)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Readable (Readable(fromText))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Language.Haskell.TH
import Data.Time
import Data.Time (ZonedTime(..))
import Data.Time.Git
import Control.Applicative

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
  parse :: (MonadPlus m, MonadIO m) => T.Text -> m (Parsed a)
  default parse :: (Readable a, MonadPlus m, MonadIO m)
                => T.Text -> m (Parsed a)
  parse = fmap Definitely . fromText
  {-# INLINE parse #-}

-- | Discard any estimate of a parse's ambiguity.
discardConfidence :: Parsed a -> a
discardConfidence (Possibly x) = x
discardConfidence (Definitely x) = x

-- | Acts just like 'fromText': tries to parse a value from a 'T.Text'
-- and discards any estimate of the parse's ambiguity.
parse' :: (MonadIO m, MonadPlus m, Parseable a) => T.Text -> m a
parse' = fmap discardConfidence . parse

instance Parseable Bool where
instance Parseable Int where
instance Parseable Float where
instance Parseable Double where
  -- Some CSV's export Doubles in a format like '1,000.00', filtering out commas lets us parse those sucessfully
  parse = fmap Definitely . fromText . T.filter (/= ',')
instance Parseable T.Text where
instance Parseable ZonedTime where
  parse txt = do
    tz <- liftIO getCurrentTimeZone
    let ts = (approxidate (T.unpack txt) :: Maybe Integer)
    let mUtcTime = posixToUTC <$> ts :: Maybe UTCTime
    let mZonedTime = utcToZonedTime tz <$> mUtcTime :: Maybe ZonedTime
    let mZonedTime' = fmap Definitely mZonedTime :: Maybe (Parsed ZonedTime)
    maybe mzero (return ) mZonedTime'


-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> Q Type
  inferType :: T.Text -> a
