{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses,
             OverloadedLabels, OverloadedStrings, TemplateHaskell,
             TypeFamilies, TypeOperators #-}
module Categorical where
import Data.Vinyl.Derived
import Data.Vinyl.XRec (toHKD)
import Frames
import Frames.Categorical (declareCategorical)
import Frames.TH (rowGenCat, RowGen(..), declarePrefixedColumn)
import Pipes (Producer, (>->))
import qualified Pipes.Prelude as P

-- * Automatically inferred categorical data types

tableTypes' (rowGenCat "test/data/catSmall.csv") { rowTypeName = "Small", tablePrefix = "S" }
tableTypes' (rowGenCat "test/data/catLarge.csv") { rowTypeName = "Large", tablePrefix = "L" }

-- Frames will, by default, generate a categorical type if there are
-- no more than 8 variants. In our small data file, five distinct
-- months appear, so a data type is generated and used.
fifthMonthSmall :: IO (Maybe SmallMonth)
fifthMonthSmall = fmap (fmap (rvalf #month)) . runSafeEffect . P.head
                $ load >-> P.drop 4
  where load :: MonadSafe m => Producer Small m ()
        load = readTableOpt smallParser "test/data/catSmall.csv"

-- When every month appears, Frames leaves that column's type as 'Text'.
fifthMonthLarge :: IO (Maybe Text)
fifthMonthLarge = fmap (fmap (rvalf #month)) . runSafeEffect . P.head
                $ load >-> P.drop 4
  where load :: MonadSafe m => Producer Large m ()
        load = readTableOpt largeParser "test/data/catLarge.csv"

-- * Custom categorical type

-- We may want to use a sum type for which not every variant appears
-- in a data file. We can specify the variants ourselves.
declareCategorical "MyMonthData" (Just "My") ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"]
declarePrefixedColumn "month" "my" ''MyMonthData

-- We could re-use the @SId@ or @LId@ types generated above, but we
-- will declare our own @MyId@ type here for the sake of completeness.
declarePrefixedColumn "id" "my" ''Int

-- This is the row type we will parse into
type MyRow = Record '[MyId, MyMonth]

-- Here we attempt to parse the @catSmall.csv@ data file into our
-- custom @MyMonthData@ type that uses three-letter abbreviations, but
-- allow our parses to fail. It turns out that @"May"@ still
-- parses. We use 'toHKD' to cut through the noise of the @(Maybe
-- :. ElField)@ interpretation we parsed into.
fifthMonthCustom :: IO (Maybe MyMonthData)
fifthMonthCustom = fmap (>>= toHKD . rgetf #month) . runSafeEffect . P.head
                 $ load >-> P.drop 4
  where load :: MonadSafe m => Producer (ColFun Maybe MyRow) m ()
        load = readTableMaybe "test/data/catSmall.csv"
