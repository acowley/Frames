{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Main (main) where
import Frames

-- This table has a column, @"drv"@, with values that Frames has in
-- the past inferred the wrong type for.

tableTypes "Mpg" "test/data/mpg.csv"

drvCol :: (Drv ∈ rs) => Record rs -> Text
drvCol = rget drv

cylCol :: (Cyl ∈ rs) => Record rs -> Int
cylCol = rget cyl

dispCol :: (Displ ∈ rs) => Record rs -> Double
dispCol = rget displ

main :: IO ()
main = return ()
