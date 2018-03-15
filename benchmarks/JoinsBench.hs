{-# LANGUAGE QuasiQuotes,
             DataKinds,
             FlexibleContexts,
             TypeApplications,
             TemplateHaskell #-}

import Frames
import Frames.Joins
import Criterion.Main

tableTypes "LCols" "data/left1.csv"
tableTypes "RCols" "data/right1.csv"
tableTypes "SmCols" "data/left_summary.csv"

lfi :: IO (Frame LCols)
lfi = inCoreAoS (readTable "data/left1.csv")

rfi :: IO (Frame RCols)
rfi = inCoreAoS (readTable "data/right1.csv")

smfi :: IO (Frame SmCols)
smfi = inCoreAoS (readTable "data/left_summary.csv")

main :: IO ()
main = do
  lf <- lfi
  rf <- rfi
  smf <- smfi
  defaultMain [
    bench "inner1a"   $ nf (innerJoin @'[PolicyID] lf) rf
    , bench "inner1b" $ nf (innerJoin @'[County] lf) smf
    , bench "inner2"  $ nf (innerJoin @'[PolicyID,County] lf) smf
    , bench "outer2"  $ nf (outerJoin @'[PolicyID,County] lf) smf
    , bench "left2"   $ nf (leftJoin  @'[PolicyID,County] lf) smf
    , bench "left2"   $ nf (rightJoin @'[PolicyID,County] lf) smf                             
    ]
    

  
