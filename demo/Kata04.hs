{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings,
             TemplateHaskell, TypeFamilies, TypeOperators,
             MultiParamTypeClasses, ScopedTypeVariables,
             FlexibleInstances #-}
module Main where
import qualified Data.Foldable as F
import Data.Ord (comparing)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vinyl.Functor (Identity(..))
import Data.Vinyl.TypeLevel (Nat(..))
import Frames

-- Data set from http://codekata.com/data/04/weather.dat
-- Associated with this problem set:
-- http://codekata.com/kata/kata04-data-munging/
tableTypes "Row" "data/weather.csv"

getTemperatureRange :: (MxT ∈ rs, MnT ∈ rs) => Record rs -> Double
getTemperatureRange row = rget mxT row - rget mnT row

partOne :: IO T.Text
partOne = do tbl <- inCoreAoS (readTable "data/weather.csv") :: IO (Frame Row)
             return $ rget dy (F.maximumBy (comparing getTemperatureRange) tbl)


-- shapr: Fight the dying of the light!

type family Find i xs where
  Find 'Z ((s :-> x) ': xs) = x
  Find ('S i) (x ': xs) = Find i xs

class GetFieldByIndex i rs where
  getFieldByIndex :: proxy i -> Record rs -> Find i rs

instance GetFieldByIndex 'Z ((s :-> r) ': rs) where
  getFieldByIndex _ (Identity x :& _) = x

instance GetFieldByIndex i rs => GetFieldByIndex ('S i) ((s :-> r) ': rs) where
  getFieldByIndex _ (_ :& xs) = getFieldByIndex (Proxy :: Proxy i) xs

getTemperatureRange' :: (GetFieldByIndex (S Z) rs, GetFieldByIndex (S (S Z)) rs,
                         Find (S Z) rs ~ Double, Find (S (S Z)) rs ~ Double )
                     => Record rs -> Double
getTemperatureRange' row = mx - mn
  where mx = getFieldByIndex (Proxy::Proxy ('S 'Z)) row
        mn = getFieldByIndex (Proxy::Proxy ('S ('S 'Z))) row

partOne' :: IO T.Text
partOne' = do tbl <- inCoreAoS (readTable "data/weather.csv") :: IO (Frame Row)
              return . getFieldByIndex (Proxy::Proxy Z) $
                F.maximumBy (comparing getTemperatureRange') tbl

main :: IO ()
main = do partOne >>= T.putStrLn
          partOne' >>= T.putStrLn
