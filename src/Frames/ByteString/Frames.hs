-- | User-friendly, type safe, runtime efficient tooling for working
-- with tabular data deserialized from comma-separated values (CSV)
-- files. The type of each row of data is inferred from data, which
-- can then be streamed from disk, or worked with in memory.
module Frames.ByteString.Frames
  ( module Data.Vinyl
  , module Data.Vinyl.Lens
  , module Frames.ByteString.Col
  , module Frames.ByteString.ColumnUniverse
  , module Frames.ByteString.CoRec
  , module Frames.ByteString.CSV
  , module Frames.ByteString.Exploration
  , module Frames.ByteString.Frame
  , module Frames.ByteString.InCore
  , module Frames.ByteString.Melt
  , module Frames.ByteString.Rec
  , module Frames.ByteString.RecF
  , module Frames.ByteString.RecLens
  , module Frames.ByteString.TypeLevel
  , Text
  ) where
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import Data.Vinyl ((<+>))
import Data.Vinyl.Lens hiding (rlens, rget, rput)
import Frames.ByteString.Col ((:->)(..))
import Frames.ByteString.ColumnUniverse
import Frames.ByteString.CoRec (Field, onField, onCoRec)
import Frames.ByteString.CSV (readTable, readTableMaybe, readTable', declareColumn,
                   tableType, tableTypes, tableType', tableTypes')
import Frames.ByteString.Exploration
import Frames.ByteString.Frame
import Frames.ByteString.InCore (toFrame, inCore, inCoreSoA,
                      inCoreAoS, inCoreAoS', toAoS, filterFrame)
import Frames.ByteString.Melt (melt, meltRow)
import Frames.ByteString.Rec (Record, RecordColumns, (&:), recUncons, recMaybe, showFields)
import Frames.ByteString.RecF
import Frames.ByteString.RecLens
import Frames.ByteString.TypeLevel
