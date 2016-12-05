{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}
module Main (manualGeneration, main) where
import Data.Monoid (First(..))
import Language.Haskell.TH
import Test.Hspec hiding (runIO)
import Frames
import DataCSV
import PrettyTH

-- | Extract all example @(CSV, generatedCode)@ pairs from
-- @test/examples.toml@
csvTests :: [(CsvExample, String)]
csvTests = $(do csvExamples <- runIO (examplesFrom "test/examples.toml")
                ListE <$> mapM (\x@(CsvExample _ c _) -> 
                                  [e|(x,$(generateCode c))|])
                               csvExamples)

-- | To generate example generated code from raw CSV data, add the csv
-- to @examples.toml@ and set the @generated@ key to an empty
-- string. Then load this file into a REPL that has @:set
-- -XTemplateHaskell@ and evaluate @putStrLn $(manualGeneration
-- "employees")@, for example.
-- 
-- Note that to load this file into a REPL may require some fiddling
-- with the path to the examples file in the 'csvTests' splice above.
manualGeneration :: String -> Q Exp
manualGeneration k = do csvExamples <- runIO (examplesFrom "test/examples.toml")
                        maybe (error ("Table " ++ k ++ " not found")) 
                              generateCode 
                              (getFirst $ foldMap aux csvExamples)
  where aux :: CsvExample -> First String
        aux (CsvExample k' c _) = if k == k' then pure c else mempty

main :: IO ()
main = do
  hspec $
    do describe "Haskell type generation" $ 
         mapM_ (\(CsvExample k _ g, g') -> it k (g' `shouldBe` g)) csvTests
