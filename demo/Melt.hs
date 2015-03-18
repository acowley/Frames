{-# LANGUAGE DataKinds, QuasiQuotes, TypeOperators #-}
module Melt where
import Control.Applicative
import Frames

type Age = "age" :-> Int
type Weight = "weight" :-> Double
type Name = "name" :-> String

testRec :: Record [Name, Age, Weight]
testRec = frameCons (pure "bob")
        $ frameCons (pure 23)
        $ frameCons (pure 75.2) Nil

testMelt :: [Record '[Name, "value" :-> Field '[Age, Weight]]]
testMelt = melt [pr1|Name|] testRec

type Val = "value" :-> Field '[Age, Weight]

-- Filter rows based on the 'Val' column. This requires applying
-- functions to values whose type is 'Field ts'.
testField :: [Record '[Name, "value" :-> Field '[Age, Weight]]]
testField = filter
              (onField [pr|Ord,Num|] (> 50) . rget (rlens [pr|Val|]))
              testMelt
