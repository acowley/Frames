{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
import Control.Applicative
import Control.Arrow ((&&&))
import Diagrams.Prelude (Diagram, R2, dims2D, width, height)
import Diagrams.Backend.Rasterific
import Data.Foldable (toList)
import Frames
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Easy

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "Row" "data/prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS $ readTable "data/prestige.csv"

mkPlot :: IO ()
mkPlot = do pts <- toList . fmap (view education &&& view income) <$> loadRows
            mkDiagram <- chart2Diagram
            let d = mkDiagram $ do
                      layout_title .= "Income vs Education"
                      layout_x_axis . laxis_title .= "Education (Years)"
                      layout_y_axis . laxis_title .= "Income ($)"
                      plot (points "" pts)
            renderRasterific "plotPrestige.png" (dims2D (width d) (height d)) d

chart2Diagram :: IO (EC (Layout Double Int) () -> Diagram Rasterific)
chart2Diagram = do env <- defaultEnv bitmapAlignmentFns 320 240
                   return $ fst . runBackendR env . toRenderable . execEC

-- [[../plotPrestige.png]]
