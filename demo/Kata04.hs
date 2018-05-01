{-# LANGUAGE AllowAmbiguousTypes, DataKinds, EmptyCase,
             FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, OverloadedStrings,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeFamilies, TypeOperators #-}
module Main where
import qualified Data.Foldable as F
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vinyl (Rec(..), ElField(..))
import Data.Vinyl.TypeLevel (Nat(..))
import Frames

-- Data set from http://codekata.com/data/04/weather.dat
-- Associated with this problem set:
-- http://codekata.com/kata/kata04-data-munging/
-- Asterisk suffixes were manually removed from temperature columns.
tableTypes "Row" "data/weather.csv"

getTemperatureRange :: (MxT ∈ rs, MnT ∈ rs) => Record rs -> Double
getTemperatureRange row = rget @MxT row - rget @MnT row

partOne :: IO T.Text
partOne = do tbl <- inCoreAoS (readTable "data/weather.csv") :: IO (Frame Row)
             return $ rget @Dy (F.maximumBy (comparing getTemperatureRange) tbl)


-- shapr: Fight the dying of the light!

type family Find i xs where
  Find 'Z ((s :-> x) ': xs) = x
  Find ('S i) (x ': xs) = Find i xs

class GetFieldByIndex i rs where
  getFieldByIndex :: Record rs -> Find i rs

instance GetFieldByIndex 'Z ((s :-> r) ': rs) where
  getFieldByIndex (Field x :& _) = x

instance GetFieldByIndex i rs => GetFieldByIndex ('S i) ((s :-> r) ': rs) where
  getFieldByIndex (_ :& xs) = getFieldByIndex @i xs

getTemperatureRange' :: (GetFieldByIndex ('S 'Z) rs,
                         GetFieldByIndex ('S ('S 'Z)) rs,
                         Find ('S 'Z) rs ~ Double,
                         Find ('S ('S 'Z)) rs ~ Double )
                     => Record rs -> Double
getTemperatureRange' row = mx - mn
  where mx = getFieldByIndex @('S 'Z) row
        mn = getFieldByIndex @('S ('S 'Z)) row

partOne' :: IO T.Text
partOne' = do tbl <- inCoreAoS (readTable "data/weather.csv") :: IO (Frame Row)
              return . getFieldByIndex @'Z $
                F.maximumBy (comparing getTemperatureRange') tbl

main :: IO ()
main = do partOne >>= T.putStrLn
          partOne' >>= T.putStrLn
