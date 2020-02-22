{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
module Issue114 where
import           Frames
import qualified Control.Foldl                 as Foldl
import           Control.Lens
import           Pipes.Prelude                  ( fold )
import           Pipes
import qualified Pipes.Prelude                 as P
import qualified Pipes.Core                    as PC

import qualified Data.Vinyl                    as V
import           Data.Vinyl                     ( Rec(..) )

import qualified Data.Text                     as T
import           Data.Attoparsec.Text
import           Control.Applicative

import           Frames.TH                      ( tableTypesText'
                                                , rowGen
                                                , RowGen(..)
                                                )

tableTypesText' (rowGen "test/data/issue114.csv") { rowTypeName = "ProductionData" }

declareColumn "oil_vol_double" ''Double

getOilVol :: ProductionData -> Record '[OilVolDouble]
getOilVol x = V.Field (f (parseOnly parseDouble (x ^. oilVol))) :& RNil
 where
  f (Left  _) = 0.0 / 0.0
  f (Right y) = y

readOilVol :: MonadSafe m => Producer (Record '[OilVolDouble]) m ()
readOilVol = (readTable "test/data/issue114.csv") >-> (P.map getOilVol)

oilVolLength :: Foldl.Fold (Record '[OilVolDouble]) Int
oilVolLength = Foldl.length

totalOilVol :: Foldl.Fold (Record '[OilVolDouble]) Double
totalOilVol = (Foldl.handles oilVolDouble) Foldl.sum

oilVolTotalAndLength :: Foldl.Fold (Record '[OilVolDouble]) (Double, Int)
oilVolTotalAndLength = (,) <$> totalOilVol <*> oilVolLength

parseDouble :: Parser Double
parseDouble =
  do
      d <- double
      return d
    <|> do
          _ <- string ""
          return 0.0

test :: IO ()
test = do
  (t, l) <- runSafeT $ Foldl.purely fold oilVolTotalAndLength readOilVol
  putStrLn $ show l ++ " records totalling " ++ show t

loadTable :: MonadSafe m => PC.Producer ProductionData m ()
loadTable = readTable "test/data/issue114.csv"

testNames :: IO [T.Text]
testNames = runSafeT . P.toListM $ loadTable >-> P.map (V.rvalf #facility_name)
