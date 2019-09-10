{-# language DataKinds, FlexibleContexts, TemplateHaskell #-}
-- | A demonstration of ingesting a CSV file, modifying the data (in
-- this case multiplying a column by 2), then writing it back out to a
-- new CSV file.
import Frames
import Frames.CSV (pipeToCSV, consumeTextLines)
import Lens.Micro
import Pipes ((>->), Effect)
import qualified Pipes.Prelude as P

tableTypes "Row" "data/data1.csv"

myFun :: Row -> Row
myFun = age %~ (*2)

myPipeline :: MonadSafe m => Effect m ()
myPipeline = readTable "data/data1.csv"
             >-> P.map myFun
             >-> pipeToCSV
             >-> consumeTextLines "data/dataMod.csv"

main :: IO ()
main = runSafeEffect myPipeline
