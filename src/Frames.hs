{-# LANGUAGE PatternSynonyms, TypeFamilies, TypeOperators #-}
-- | User-friendly, type safe, runtime efficient tooling for working
-- with tabular data deserialized from comma-separated values (CSV)
-- files. The type of each row of data is inferred from data, which
-- can then be streamed from disk, or worked with in memory.
module Frames
  ( module Data.Readable
  , module Data.Vinyl
  , module Data.Vinyl.CoRec
  , module Data.Vinyl.Derived
  , module Data.Vinyl.Functor
  , module Data.Vinyl.Lens
  , module Data.Vinyl.TypeLevel
  , module Frames.Col
  , module Frames.ColumnUniverse
  , module Frames.CSV
  , module Frames.Exploration
  , module Frames.Frame
  , inCoreAoS, inCoreAoS', inCore, inCoreSoA
  , I.toAoS, I.toFrame, I.filterFrame
  , module Frames.Joins
  , module Frames.Melt
  , module Frames.Rec
  , module Frames.RecF
  , module Frames.TH
  , module Frames.TypeLevel
  , module Pipes.Safe, runSafeEffect
  , Text
  ) where
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive
import Data.Readable (Readable(..))
import Data.Text (Text)
import Data.Vinyl ((<+>), Rec, rcast, rsubset, ElField)
import Data.Vinyl.CoRec (Field, onField, onCoRec)
import Data.Vinyl.Derived (rfield)
import Data.Vinyl.Functor ((:.))
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel (AllConstrained, AllSatisfied, AllAllSat,
                             RDelete, RecAll)
import Frames.Col ((:->), pattern Col)
import Frames.ColumnUniverse
import Frames.CSV (readTable, readTableOpt, readTableMaybe, pipeTable, pipeTableMaybe)
import Frames.Exploration
import Frames.Frame
import qualified Frames.InCore as I
import Frames.Melt (melt, meltRow)
import Frames.Rec (Record, RecordColumns, (&:), recUncons, recMaybe, showFields)
import Frames.Rec (rgetField, rputField)
import Frames.RecF
import Frames.TH (tableTypes, tableTypes', declareColumn)
import Frames.TypeLevel
import Frames.Joins
import Frames.ExtraInstances()
import qualified Pipes as P
import Pipes.Safe (MonadSafe, runSafeT, runSafeP, SafeT)
import qualified Pipes.Safe as PS

-- * SafeT helpers

-- | Run a self-contained ’Pipes.Effect’ and execute the finalizers
-- associated with the ’SafeT’ transformer.
runSafeEffect :: (MonadIO m, PS.MonadMask m)
              => P.Effect (SafeT m) r -> m r
runSafeEffect = runSafeT . P.runEffect

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generators a matter of indexing into a densely packed
-- representation. Returns a 'Frame' that provides a function to index
-- into the table.
inCoreAoS :: (PrimMonad m, MonadIO m, PS.MonadMask m, I.RecVec rs)
          => P.Producer (Record rs) (PS.SafeT m) () -> m (FrameRec rs)
inCoreAoS = runSafeT . I.inCoreAoS

-- | Like 'inCoreAoS', but applies the provided function to the record
-- of columns before building the 'Frame'.
inCoreAoS' :: (PrimMonad m, MonadIO m, PS.MonadMask m, I.RecVec rs)
           => (Rec ((->) Int :. ElField) rs -> Rec ((->) Int :. ElField) ss)
           -> P.Producer (Record rs) (SafeT m) () -> m (FrameRec ss)
inCoreAoS' f = runSafeT . I.inCoreAoS' f

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generator a matter of indexing into a densely packed
-- representation.
inCore :: (PrimMonad m, MonadIO m, PS.MonadMask m, I.RecVec rs, Monad n)
       => P.Producer (Record rs) (SafeT m) () -> m (P.Producer (Record rs) n ())
inCore = runSafeT . I.inCore

-- | Stream a finite sequence of rows into an efficient in-memory
-- representation for further manipulation. Each column of the input
-- table will be stored optimally based on its type, making use of the
-- resulting generators a matter of indexing into a densely packed
-- representation. Returns the number of rows and a record of column
-- indexing functions. See 'toAoS' to convert the result to a 'Frame'
-- which provides an easier-to-use function that indexes into the
-- table in a row-major fashion.
inCoreSoA :: (PrimMonad m, MonadIO m, PS.MonadMask m, I.RecVec rs)
          => P.Producer (Record rs) (SafeT m) () -> m (Int, Rec ((->) Int :. ElField) rs)
inCoreSoA = runSafeT . I.inCoreSoA
