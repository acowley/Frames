{-# LANGUAGE BangPatterns, DefaultSignatures, LambdaCase #-}
module Frames.ColumnTypeable where
import Control.Monad (MonadPlus, mzero)
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH
import Data.Time
import Data.Time (LocalTime(..))
import Data.Time.Zones.DB
import Data.Time.Zones
import Data.Time.Zones.Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

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
  parse :: MonadPlus m => String -> T.Text -> m (Parsed a)
  default parse :: (Readable a, MonadPlus m)
                => String -> T.Text -> m (Parsed a)
  parse _ txt = fmap Definitely . fromText $ txt
  {-# INLINE parse #-}

-- | Discard any estimate of a parse's ambiguity.
discardConfidence :: Parsed a -> a
discardConfidence (Possibly x) = x
discardConfidence (Definitely x) = x

-- | Acts just like 'fromText': tries to parse a value from a 'T.Text'
-- and discards any estimate of the parse's ambiguity.
parse' :: (MonadPlus m, Parseable a) => String -> T.Text -> m a
parse' tz txt = do
  let parsed = parse tz txt
  fmap discardConfidence parsed

instance Parseable Bool where
instance Parseable Int where
instance Parseable Float where
instance Parseable Double where
  -- Some CSV's export Doubles in a format like '1,000.00', filtering out commas lets us parse those sucessfully
  parse tz txt = fmap Definitely . fromText . T.filter (/= ',') $ txt
instance Parseable T.Text where
instance Parseable ZonedTime where
  parse tz txt = do
    case (parseDateByTZString (C8.pack tz) (T.unpack txt) :: Maybe ZonedTime) of
      Just zt -> pure $ Definitely zt
      Nothing -> mzero

-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> Q Type
  inferType :: T.Text -> a

parseDateByTZString
  :: (Monad m, MonadPlus m) => BS.ByteString -> String -> m ZonedTime
parseDateByTZString tzString dateString = do
    let mLocalTime = parseTimeM True defaultTimeLocale "%F %T" dateString

    -- getting raw tz data from label
    let mTzRawBS = tzDataByLabel <$> fromTZName tzString

    case mTzRawBS of
      Just tzRawBS -> do
        let tz = parseOlson tzRawBS
        let mUtcTime = localTimeToUTCTZ tz <$> mLocalTime
        let timeZoneAsOfDate = timeZoneForUTCTime tz <$> mUtcTime
        ZonedTime <$> mLocalTime <*> timeZoneAsOfDate
      Nothing -> mzero
