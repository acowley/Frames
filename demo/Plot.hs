{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell, TypeApplications #-}
module Main where
import qualified Data.Vector.Unboxed as V
import Diagrams.Backend.Rasterific
import Diagrams (dims2D, width, height)
import Frames
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Easy
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Statistics.Sample.KernelDensity (kde)

-- Data from http://wwwn.cdc.gov/nchs/nhanes/2005-2006/TRIGLY_D.htm
tableTypes "Trigly" "data/trigly_d.csv"

-- Load the data. Invalid records use zeros as a placeholder.
triglyData :: MonadSafe m => P.Producer Trigly m ()
triglyData = readTable "data/trigly_d.csv" P.>-> P.filter ((> 0) . view lBDLDL)

-- Adapted from a Chart example
fillBetween :: String -> [(a, (b, b))] -> EC l (PlotFillBetween a b)
fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- dissolve 0.5 `fmap` takeColor
  plot_fillbetween_style .= solidFillStyle color
  plot_fillbetween_values .= vs

-- | Plot a semi-transparent KDE with a thick black outline.
mkPlot :: String -> [Int] -> EC (Layout Double Double) ()
mkPlot title xs = do plot (fillBetween title pts)
                     plot . liftEC $ do
                       zoom plot_lines_style $ do
                         line_color .= opaque black
                         line_width *= 2
                       plot_lines_values .= [map (_2 %~ snd) pts]
  where pts = V.toList . uncurry (V.zipWith (\x y -> (x, (0, y)))) . kde 128
            $ V.fromList (map fromIntegral xs)

-- | Plot LDL cholesterol and Triglyceride levels.
mkPlots :: [Record [LBXTR, LBDLDL]] -> EC (Layout Double Double) ()
mkPlots xs = do layout_title .= "Distributions"
                layout_x_axis . laxis_title .= "mg/dL"
                layout_all_font_styles . font_size *= 2
                mkPlot "LDL" ldls
                mkPlot "Triglycerides" tris
  where ldls = map (view lBDLDL) xs
        tris = map (view lBXTR) xs

main :: IO ()
main = do env <- defaultEnv bitmapAlignmentFns 640 480
          let chart2diagram = fst . runBackendR env . toRenderable . execEC
          ldlData <- runSafeT . P.toListM $ triglyData P.>-> P.map rcast
          let d = chart2diagram $ mkPlots ldlData
              sz = dims2D (width d) (height d)
          renderRasterific "plot.png" sz d
