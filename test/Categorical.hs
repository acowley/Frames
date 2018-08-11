{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedLabels, TemplateHaskell #-}
module Categorical where
import Data.String (IsString(..))
import Data.Vinyl.Derived
import Frames
import Frames.CSV
import Pipes (Producer, (>->))
import qualified Pipes.Prelude as P

tableTypes' (rowGenCat "test/data/catSmall.csv") { rowTypeName = "Small", tablePrefix = "S" }
tableTypes' (rowGenCat "test/data/catLarge.csv") { rowTypeName = "Large", tablePrefix = "L" }

fifthMonthSmall :: IO (Maybe SmallMonth)
fifthMonthSmall = fmap (fmap (rvalf #month)) . runSafeEffect . P.head
                $ load >-> P.drop 4
  where load :: MonadSafe m => Producer Small m ()
        load = readTableOpt smallParser "test/data/catSmall.csv"

fifthMonthLarge :: IO (Maybe Text)
fifthMonthLarge = fmap (fmap (rvalf #month)) . runSafeEffect . P.head
                $ load >-> P.drop 4
  where load :: MonadSafe m => Producer Large m ()
        load = readTableOpt largeParser "test/data/catLarge.csv"
