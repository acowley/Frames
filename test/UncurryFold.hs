{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell, TypeApplications #-}
module UncurryFold where
import qualified Control.Foldl                 as L
import           Data.Vinyl.Curry               ( runcurryX )
import           Frames

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "Row" "test/data/prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS (readTable "test/data/prestige.csv")

-- | Compute the ratio of income to prestige for a record containing
-- only those fields.
ratio :: Record '[Income, Prestige] -> Double
ratio = runcurryX (\i p -> fromIntegral i / p)

averageRatio :: IO Double
averageRatio = L.fold (L.premap (ratio . rcast) avg) <$> loadRows
  where avg = (/) <$> L.sum <*> L.genericLength
