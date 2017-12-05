{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
import qualified Control.Foldl as L
import Data.Vinyl.Curry (runcurry')
import Data.Vinyl (rcast)
import Frames

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "Row" "data/prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS (readTable "data/prestige.csv")

ratio :: Record '[Income, Prestige] -> Double
ratio = runcurry' go . toVinyl
  where go i p = fromIntegral i / p

averageRatio :: IO Double
averageRatio = L.fold (L.premap (ratio . rcast) go) <$> loadRows
  where go = (/) <$> L.sum <*> L.genericLength
