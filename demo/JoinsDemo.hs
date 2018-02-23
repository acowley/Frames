{-# LANGUAGE QuasiQuotes,
             DataKinds,
             FlexibleContexts,
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
  print $ length $ innerJoin [pr1|PolicyID|] lf rf
  print $ length $ innerJoin [pr1|PolicyID|] lf smf
  print $ length $ innerJoin [pr|PolicyID,County|] lf smf
  print $ length $ outerJoin [pr|PolicyID,County|] lf smf
  print $ length $ leftJoin  [pr|PolicyID,County|] lf smf
  print $ length $ rightJoin [pr|PolicyID,County|] lf smf                             

    

  
