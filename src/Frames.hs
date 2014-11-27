-- | User-friendly, type safe, runtime efficient tooling for working
-- with tabular data deserialized from comma-separated values (CSV)
-- files. The type of each row of data is inferred from data, which
-- can then be streamed from disk, or worked with in memory.
module Frames
  ( module Frames.Col
  , module Frames.CSV
  , module Frames.InCore
  , module Frames.Rec
  , module Frames.RecF
  , module Frames.RecLens
  , module Frames.TypeLevel
  , Text
  ) where
import Data.Text (Text)
import Frames.Col ((:->)(..))
import Frames.CSV (readTable, readTableMaybe, readTable', 
                   tableType, tableTypes, tableTypesPrefixed)
import Frames.InCore (Frame(..), FrameRec, inCore, inCoreSoA,
                      inCoreAoS, inCoreAoS', toAoS)
import Frames.Rec (Rec, (&:), recPure, recMaybe, showRec, All)
import Frames.RecF
import Frames.RecLens hiding (lens)
import Frames.TypeLevel hiding (RIndex, RImage)
