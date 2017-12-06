{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
import qualified Control.Foldl as L
import Data.Vinyl (rcast)
import Frames
import Frames.CSV (rowGen, columnNames, tablePrefix, rowTypeName)

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes' (rowGen "data/prestigeNoHeader.csv")
            { rowTypeName = "NoH"
            , columnNames = [ "Job", "Schooling", "Money", "Females"
                            , "Respect", "Census", "Category" ]
            , tablePrefix = "NoHead"}

loadRows :: IO (Frame NoH)
loadRows = inCoreAoS (readTable "data/prestigeNoHeader.csv")

-- | Compute the ratio of money to respect for a record containing
-- only those fields.
ratio :: Record '[NoHeadMoney, NoHeadRespect] -> Double
ratio = runcurry' (\m r -> fromIntegral m / r)

averageRatio :: IO Double
averageRatio = L.fold (L.premap (ratio . rcast) avg) <$> loadRows
  where avg = (/) <$> L.sum <*> L.genericLength
