{-# LANGUAGE QuasiQuotes,
             DataKinds,
             FlexibleContexts,
             TypeApplications,
             TemplateHaskell #-}

import Frames
import Frames.Joins

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
  print $ length $ innerJoin @'[PolicyID] lf rf
  print $ length $ innerJoin @'[PolicyID] lf smf
  print $ length $ innerJoin @'[PolicyID,County] lf smf
  print $ length $ outerJoin @'[PolicyID,County] lf smf
  print $ length $ leftJoin  @'[PolicyID,County] lf smf
  print $ length $ rightJoin @'[PolicyID,County] lf smf                             

    

  
