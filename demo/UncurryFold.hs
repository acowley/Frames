{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
import qualified Control.Foldl as L
import Data.Vinyl (rcast)
import Frames

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "Row" "data/prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS (readTable "data/prestige.csv")

-- | Compute the ratio of income to prestige for a record containing
-- only those fields.
ratio :: Record '[Income, Prestige] -> Double
ratio = runcurry' (\i p -> fromIntegral i / p)

averageRatio :: IO Double
averageRatio = L.fold (L.premap (ratio . rcast) go) <$> loadRows
  where go = (/) <$> L.sum <*> L.genericLength
