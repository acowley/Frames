-- | User-friendly, type safe, runtime efficient tooling for working
-- with tabular data deserialized from comma-separated values (CSV)
-- files. The type of each row of data is inferred from data, which
-- can then be streamed from disk, or worked with in memory.
module Frames
  ( module Data.Vinyl
  , module Data.Vinyl.Lens
  , module Frames.Col
  , module Frames.ColumnUniverse
  , module Frames.CoRec
  , module Frames.CSV
  , module Frames.Exploration
  , module Frames.Frame
  , module Frames.InCore
  , module Frames.Melt
  , module Frames.Rec
  , module Frames.RecF
  , module Frames.RecLens
  , module Frames.TypeLevel
  , Text
  , ZonedTime
  ) where
import Data.Text (Text)
import Data.Vinyl ((<+>))
import Data.Vinyl.Lens hiding (rlens, rget, rput)
import Frames.Col ((:->)(..))
import Frames.ColumnUniverse
import Frames.CoRec (Field, onField, onCoRec)
import Frames.CSV (readTable, readTableMaybe, readTable', declareColumn,
                   tableType, tableTypes, tableType', tableTypes')
import Frames.Exploration
import Frames.Frame
import Frames.InCore (toFrame, inCore, inCoreSoA,
                      inCoreAoS, inCoreAoS', toAoS, filterFrame)
import Frames.Melt (melt, meltRow)
import Frames.Rec (Record, RecordColumns, (&:), recUncons, recMaybe, showFields)
import Frames.RecF
import Frames.RecLens
import Frames.TypeLevel
import Data.Time
