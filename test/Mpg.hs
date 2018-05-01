{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Main (main) where
import Frames

-- This table has a column, @"drv"@, with values that Frames has in
-- the past inferred the wrong type for.

tableTypes "Mpg" "test/data/mpg.csv"

drvCol :: (Drv ∈ rs) => Record rs -> Text
drvCol = rget @Drv

cylCol :: (Cyl ∈ rs) => Record rs -> Int
cylCol = rget @Cyl

dispCol :: (Displ ∈ rs) => Record rs -> Double
dispCol = rget @Displ

main :: IO ()
main = return ()
