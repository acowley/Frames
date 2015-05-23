{-# LANGUAGE DataKinds, QuasiQuotes, TypeOperators #-}
module Melt where
import Control.Applicative
import Data.Foldable (toList)
import Frames

type Age = "age" :-> Int
type Weight = "weight" :-> Double
type Name = "name" :-> String

testRec1 :: Record '[Name, Age, Weight]
testRec1 = frameCons (pure "bob")
         $ frameCons (pure 23)
         $ frameCons (pure 75.2) Nil

testMelt :: Record '[Name, Age, Weight]
         -> [Record '[Name, "value" :-> Field '[Age, Weight]]]
testMelt = meltRow [pr1|Name|]

type Val = "value" :-> Field '[Age, Weight]

-- Filter rows based on the 'Val' column. This requires applying
-- functions to values whose type is 'Field ts'.
testField :: [Record '[Name, "value" :-> Field '[Age, Weight]]]
testField = filter
              (onField [pr|Ord,Num|] (> 50) . rget (rlens [pr|Val|]))
              (testMelt testRec1)

testRec2 :: Record '[Name, Age, Weight]
testRec2 = frameCons (pure "alice")
         $ frameCons (pure 34)
         $ frameCons (pure 55.2) Nil

molten :: [Record '[Name, "value" :-> Field '[Age, Weight]]]
molten = toList (melt [pr1|Name|] (toFrame [testRec1, testRec2]))
