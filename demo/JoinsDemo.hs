{-# LANGUAGE QuasiQuotes,
             DataKinds,
             FlexibleContexts,
             TemplateHaskell #-}

import Criterion.Main
import Frames
import Frames.Joins
import Data.Foldable as F

tableTypes "LCols" "data/left1.csv"
tableTypes "RCols" "data/right1.csv"

lfi :: IO (Frame LCols)
lfi = inCoreAoS (readTable "data/left1.csv")

rfi :: IO (Frame RCols)
rfi = inCoreAoS (readTable "data/right1.csv")

main :: IO ()
main = do
  lf <- lfi
  rf <- rfi
  let rf2 = toFrame $ reverse $ toList rf in
    let x = innerJoin [pr1|PolicyID|] lf rf 
        y = innerJoin [pr1|PolicyID|] lf rf2 in
      print (x == y)

  
  

