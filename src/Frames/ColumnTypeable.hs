-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- module Frames.ColumnTypeable where
-- import Data.Time.Zones.DB
-- import Data.Time.Zones
-- import Data.Time.Zones.TH
-- import Data.Time
-- import qualified Data.ByteString as BS

-- timeExample :: BS.ByteString -> IO ()
-- timeExample tzString = do
--     $(includeTZFromDB tzString)
    -- let (Just tz) = fromTZName tzString
    -- let dtTz = 
    -- print tz
    -- let (Just lt) = parseTimeM True defaultTimeLocale "%F %T" "2016-02-01 03:00:00" :: Maybe LocalTime
    -- let cstTz = hoursToTimeZone (- 6)
    -- let cdtTz = hoursToTimeZone (- 5)
    -- let utcTime = localTimeToUTC cstTz lt
    -- print lt



{-# LANGUAGE BangPatterns, DefaultSignatures, LambdaCase #-}
module Frames.ColumnTypeable where
import Control.Monad (MonadPlus)
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import Language.Haskell.TH
import Data.Time
import Data.Time (LocalTime(..))

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
  parse :: MonadPlus m => T.Text -> m (Parsed a)
  default parse :: (Readable a, MonadPlus m)
                => T.Text -> m (Parsed a)
  parse = fmap Definitely . fromText
  {-# INLINE parse #-}

-- | Discard any estimate of a parse's ambiguity.
discardConfidence :: Parsed a -> a
discardConfidence (Possibly x) = x
discardConfidence (Definitely x) = x

-- | Acts just like 'fromText': tries to parse a value from a 'T.Text'
-- and discards any estimate of the parse's ambiguity.
parse' :: (MonadPlus m, Parseable a) => T.Text -> m a
parse' = fmap discardConfidence . parse

instance Parseable Bool where
instance Parseable Int where
instance Parseable Float where
instance Parseable Double where
  -- Some CSV's export Doubles in a format like '1,000.00', filtering out commas lets us parse those sucessfully
  parse = fmap Definitely . fromText . T.filter (/= ',')
instance Parseable T.Text where
instance Parseable LocalTime where
  parse txt = do
    fmap Definitely (parseTimeM True defaultTimeLocale "%F %T" (T.unpack txt ))
    -- case (parseTimeM True defaultTimeLocale "%F %T" (T.unpack txt ):: Maybe ZonedTime) of
    --   Just zt -> pure $ Definitely zt
    --   Nothing -> error "Hm. not sure what to do here"

-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> Q Type
  inferType :: T.Text -> a

-- timeExample = do
--     let (Just lt) = parseTimeM True defaultTimeLocale "%F" "2016-02-01" :: Maybe LocalTime
--     print lt
