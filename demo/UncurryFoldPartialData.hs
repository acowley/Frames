{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
import qualified Control.Foldl as L
import Data.Maybe (isNothing)
import Frames
import Pipes (Producer, (>->))
import qualified Pipes.Prelude as P

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
-- The prestige column has been left blank for rows whose "type" is
-- listed as "NA".
tableTypes "Row" "data/prestigePartial.csv"

-- | A pipes 'Producer' of our 'Row' type with a column functor
-- ('ColFun') of 'Maybe'. That is, each element of each row may have
-- failed to parse from the CSV file.
maybeRows :: MonadSafe m => Producer (ColFun Maybe Row) m ()
maybeRows = readTableMaybe "data/prestigePartial.csv"

-- | Return the number of rows with unknown prestige, and the average
-- income of those rows.
incomeOfUnknownPrestige :: IO (Int, Double)
incomeOfUnknownPrestige =
  runSafeEffect . L.purely P.fold avg $
    maybeRows >-> P.filter prestigeUnknown >-> P.map getIncome >-> P.concat
  where avg = (\s l -> (l, s / fromIntegral l)) <$> L.sum <*> L.length
        getIncome = fmap fromIntegral . rget' income'
        prestigeUnknown = isNothing . rget' prestige'
