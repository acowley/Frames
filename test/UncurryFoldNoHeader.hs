{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell, TypeApplications #-}
module UncurryFoldNoHeader where
import qualified Control.Foldl                 as L
import           Data.Vinyl.Curry               ( runcurryX )
import           Frames
import           Frames.TH                      ( rowGen
                                                , RowGen(..)
                                                )

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes' (rowGen "test/data/prestigeNoHeader.csv")
            { rowTypeName = "NoH"
            , columnNames = [ "Job", "Schooling", "Money", "Females"
                            , "Respect", "Census", "Category" ]
            , tablePrefix = "NoHead"}

loadRows :: IO (Frame NoH)
loadRows = inCoreAoS (readTableOpt noHParser "test/data/prestigeNoHeader.csv")

-- | Compute the ratio of money to respect for a record containing
-- only those fields.
ratio :: Record '[NoHeadMoney, NoHeadRespect] -> Double
ratio = runcurryX (\m r -> fromIntegral m / r)

averageRatio :: IO Double
averageRatio = L.fold (L.premap (ratio . rcast) avg) <$> loadRows
  where avg = (/) <$> L.sum <*> L.genericLength
