{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- | Define the 'TimeIn' type that lets us specify in the type how a
-- 'LocalTime' should be converted to a 'UTCTime'.
module TimeIn where
import Control.Monad (MonadPlus (mzero), msum)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.TH
import Frames.ColumnTypeable (Parseable(..), Parsed(..))
import GHC.TypeLits
import Language.Haskell.TH

-- | A 'UTCTime' tagged with a symbol denoting the 'TZ' time zone from
-- whence it came.
newtype TimeIn (zone :: Symbol) = TimeIn UTCTime deriving Show

failZero :: MonadPlus m => Maybe r -> m r
failZero = maybe mzero pure

-- | Try to parse a 'LocalTime' value using common formats.
parseLocalTime :: MonadPlus m => T.Text -> m LocalTime
parseLocalTime t = msum (map (($ T.unpack t) . mkParser) formats)
  where formats = ["%F %T", "%F"]
        mkParser = (failZero .) . parseTimeM True defaultTimeLocale

-- | @zonedTime "America/Chicago"@ will create a 'Parseable' instance
-- for the type @TimeIn "America/Chicago"@. You can then use this type
-- when loading data.
timeIn :: String -> DecsQ
timeIn tzStr =
  do let fromLocal = [e| localTimeToUTCTZ $(includeTZFromDB tzStr) |]
         ex = [e| fmap (Definitely . TimeIn . $fromLocal) . parseLocalTime |]
     sequenceA [
       instanceD (pure [])
                 [t|Parseable (TimeIn $(pure $ LitT (StrTyLit tzStr)))|]
                 [ funD (mkName "parse") [clause [] (normalB ex) []] ] ]
