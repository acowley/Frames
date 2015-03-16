{-# LANGUAGE BangPatterns, DataKinds, FlexibleContexts, OverloadedStrings,
             TemplateHaskell #-}
import Diagrams.Backend.Rasterific
import Diagrams.TwoD.Size (sizeSpec2D)
import Frames
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Easy
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Text as T
import Control.Arrow ((&&&))
import qualified Data.Foldable as F

-- Data from http://archive.ics.uci.edu/ml/datasets/Adult
tableTypes "Income" "data/adult.csv"

adultData :: Producer Income IO ()
adultData = readTable "data/adult.csv"

fishers :: Producer Income IO ()
fishers = adultData >-> P.filter isFisher >-> P.filter makesMoney
  where isFisher = ((>0) . T.count "fishing" . T.toCaseFold . view occupation)
        makesMoney = (> 0) . view capitalGain

fisherIncomeData :: Producer (Record [Age, CapitalGain]) IO ()
fisherIncomeData = fishers >-> P.map rcast

mkPlot :: IO ()
mkPlot = do env <- defaultEnv bitmapAlignmentFns 640 480
            let chart2diagram = fst . runBackendR env . toRenderable . execEC
            xs <- P.toListM fisherIncomeData
            let d = chart2diagram $ do
                      layout_title .= "Farmer/fisher Income vs Age"
                      layout_x_axis . laxis_title .= "Age (Years)"
                      layout_y_axis . laxis_title .= "Capital Gain ($)"
                      plot (points "" (map (view age &&& view capitalGain) xs))
            renderRasterific "plot2.png" (sizeSpec2D d) 100 d

-- Manually fused folds
main :: IO ()
main = do ((age_,inc,n), _) <- P.fold' aux (0,0,0::Double) id fisherIncomeData
          putStrLn $ "The average farmer/fisher is "++
                     show (fromIntegral age_ / n) ++
                     " and made " ++ show (fromIntegral inc / n)
  where aux !(!sumAge, !sumIncome, n) f = (sumAge + f^.age, sumIncome + f^.capitalGain, n+1)

-- Independent folds
maiN :: IO ()
maiN = do frames <- inCoreAoS fisherIncomeData
          let age_ = F.foldl' ((. view age) . (+)) 0 frames
              inc = F.foldl' ((. view capitalGain) . (+)) 0 frames
              n = fromIntegral $ frameLength frames :: Double
          putStrLn $ "The average farmer/fisher is "++
                     show (fromIntegral age_ / n) ++
                     " and made " ++ show (fromIntegral inc / n)
