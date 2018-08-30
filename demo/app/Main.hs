{-# LANGUAGE BangPatterns, DataKinds, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, TypeOperators #-}
module Main where
import Data.Functor.Identity
import Frames
import Lens.Micro
import qualified Pipes as P
import qualified Pipes.Prelude as P

tableTypes "Row" "../data/data1.csv"

tbl :: IO [Row]
tbl = runSafeT . P.toListM $ readTable "../data/data1.csv"


ageDoubler :: (Age ∈ rs) => Record rs -> Record rs
ageDoubler = age %~ (* 2)

tbl2 :: IO [Row]
tbl2 = runSafeT . P.toListM $ readTable "../data/data2.csv"

tbl2a :: IO [ColFun Maybe Row]
tbl2a = runSafeT . P.toListM $ readTableMaybe "../data/data2.csv"

-- Sample data from http://support.spatialkey.com/spatialkey-sample-csv-data/
-- Note: We have to replace carriage returns (\r) with line feed
-- characters (\n) for the text library's line parsing to work.
tableTypes "Ins" "../data/FL2.csv"

insuranceTbl :: MonadSafe m => P.Producer Ins m ()
insuranceTbl = readTable "../data/FL2.csv"

insMaybe :: MonadSafe m => P.Producer (ColFun Maybe Ins) m ()
insMaybe = readTableMaybe "../data/FL2.csv"

type TinyIns = Record [PolicyID, PointLatitude, PointLongitude]

main :: IO ()
main = do itbl <- inCore $ P.for insuranceTbl (P.yield . rcast)
            :: IO (P.Producer TinyIns Identity ())
          putStrLn "In-core representation prepared"
          let Identity (n,sumLat) =
                P.fold (\ !(!i,!s) r -> (i+1, s+rgetField @PointLatitude r))
                       (0::Int,0)
                       id
                       itbl
          putStrLn $ "Considering " ++ show n ++ " records..."
          putStrLn $ "Average latitude: " ++ show (sumLat / fromIntegral n)
          let Identity sumLong =
                P.fold (\ !s r -> (s + rgetField @PointLongitude r)) 0 id itbl
          putStrLn $ "Average longitude: " ++ show (sumLong / fromIntegral n)
