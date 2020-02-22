{-# LANGUAGE BangPatterns, DataKinds, FlexibleContexts, OverloadedStrings,
             TemplateHaskell, TypeApplications #-}
import Diagrams.Backend.Rasterific
import Diagrams (dims2D, width, height)
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

adultData :: MonadSafe m => Producer Income m ()
adultData = readTable "data/adult.csv"

fishers :: MonadSafe m => Producer Income m ()
fishers = adultData >-> P.filter isFisher >-> P.filter makesMoney
  where isFisher = ((>0) . T.count "fishing" . T.toCaseFold . view occupation)
        makesMoney = (> 0) . view capitalGain

fisherIncomeData :: MonadSafe m => Producer (Record [Age, CapitalGain]) m ()
fisherIncomeData = fishers >-> P.map rcast

mkPlot :: IO ()
mkPlot = do env <- defaultEnv bitmapAlignmentFns 640 480
            let chart2diagram = fst . runBackendR env . toRenderable . execEC
            xs <- runSafeT $ P.toListM fisherIncomeData
            let d = chart2diagram $ do
                      layout_title .= "Farmer/fisher Income vs Age"
                      layout_x_axis . laxis_title .= "Age (Years)"
                      layout_y_axis . laxis_title .= "Capital Gain ($)"
                      plot (points "" (map (view age &&& view capitalGain) xs))
                sz = dims2D (width d) (height d)
            renderRasterific "plot2.png" sz d

-- Manually fused folds
main :: IO ()
main = do ((age_,inc,n), _) <- runSafeT $
                               P.fold' aux (0,0,0::Double) id fisherIncomeData
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
