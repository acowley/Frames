{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
import Control.Applicative
import Control.Arrow ((&&&))
import Diagrams.Prelude (Diagram, R2, sizeSpec2D)
import Diagrams.Backend.Rasterific
import Data.Foldable (toList)
import Frames
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Easy

tableTypes "Row" "/tmp/prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS $ readTable "/tmp/prestige.csv"

mkPlot :: IO ()
mkPlot = do pts <- toList . fmap (view education &&& view income) <$> loadRows
            mkDiagram <- chart2Diagram
            let d = mkDiagram $ do
                      layout_title .= "Income vs Education"
                      layout_x_axis . laxis_title .= "Education (Years)"
                      layout_y_axis . laxis_title .= "Income ($)"
                      plot (points "" pts)
            renderRasterific "plotPrestige.png" (sizeSpec2D d) 100 d

chart2Diagram :: IO (EC (Layout Double Int) () -> Diagram Rasterific R2)
chart2Diagram = do env <- defaultEnv bitmapAlignmentFns 320 240
                   return $ fst . runBackendR env . toRenderable . execEC

-- [[../plotPrestige.png]]
