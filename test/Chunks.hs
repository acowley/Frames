{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes,
             TemplateHaskell, TypeApplications #-}
module Chunks where
import Frames
import Frames.InCore (frameChunks, produceFrameChunks)
import Pipes ((>->))
import qualified Pipes.Prelude as P

tableTypes "Row" "test/data/prestige.csv"

getEducation :: Row -> Double
getEducation = rgetField @Education

chunkInCore :: IO [Double]
chunkInCore = do
  rows <- inCoreAoS (readTable "test/data/prestige.csv")
  let chunks = frameChunks 10 rows
  return $ map (getEducation . flip frameRow 0) chunks

chunkStream :: IO [Double]
chunkStream = runSafeT . P.toListM $
   produceFrameChunks 10 (readTable "test/data/prestige.csv")
   >-> P.map (getEducation . flip frameRow 0)
