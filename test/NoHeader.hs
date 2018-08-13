{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedLabels, TemplateHaskell   #-}
module NoHeader where
import Control.Arrow ((&&&))
import Data.Vinyl.Derived
import Frames
import Frames.TH (rowGen, RowGen(..))
import Pipes (Producer, (>->))
import qualified Pipes.Prelude as P

tableTypes' (rowGen "test/data/prestigeNoHeader.csv")
            { rowTypeName = "Row"
            , columnNames = [ "job"
                            , "schooling"
                            , "money"
                            , "females"
                            , "respect"
                            , "census"
                            , "category" ] }

loadData :: MonadSafe m => Producer Row m ()
loadData = readTableOpt rowParser "test/data/prestigeNoHeader.csv"

-- | Extract the @Job@ and @Schooling@ columns of the indicated row.
getJobAndSchooling :: Int -> IO (Maybe (Text, Double))
getJobAndSchooling n = fmap (fmap aux) . runSafeEffect . P.head
                     $ loadData >-> P.drop (max n 0)
  where aux :: Row -> (Text, Double)
        aux = rvalf #job &&& rvalf #schooling
