{-# LANGUAGE CPP, DataKinds, OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeOperators #-}
module Main (manualGeneration, main) where
import Control.Exception (ErrorCall, catch)
import Control.Monad (unless)
import Data.Functor.Identity
import Data.Char
import qualified Data.Foldable as F
import Data.List (find, isPrefixOf)
import Data.Monoid (First(..))
import qualified Data.Text as T
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Frames
import Frames.CSV (produceCSV)
import Frames.CSV (defaultParser, produceTokens, defaultSep, readColHeaders)
import qualified Chunks
import DataCSV
import Pipes.Prelude (toListM)
import PrettyTH
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec as H
import Test.HUnit.Lang (assertFailure)

import qualified LatinTest as Latin
import qualified Issue114 as Issue114
import qualified NoHeader
import qualified Categorical

import qualified UncurryFold
import qualified UncurryFoldNoHeader
import qualified UncurryFoldPartialData

-- | Extract all example @(CSV, generatedCode)@ pairs from
-- @test/examples.toml@
csvTests :: [(CsvExample, String)]
csvTests = $(do addDependentFile "test/examples.toml"
                csvExamples <- TH.runIO (examplesFrom "test/examples.toml")
                ListE <$> mapM (\x@(CsvExample _ c _) ->
                                  [e|(x,$(generateCode "Row" c))|])
                               csvExamples)

-- | Detect type-compatible re-used names and do not attempt to
-- re-generate definitions for them. This does not do the right thing
-- since the generated declarations are never turned into actual
-- declarations, they do not affect the `lookupTypeName` call that is
-- designed to prevent duplicate declarations.
-- overlappingGeneration :: String
-- overlappingGeneration = m ++ "\n\n" ++ e
--   where m = $(do csvExamples <- TH.runIO (examplesFrom "test/examples.toml")
--                  let Just (CsvExample _ managers _) =
--                        find (\(CsvExample k _ _) -> k == "managers") csvExamples
--                  generateCode "ManagerRec" managers)
--         e = $(do csvExamples <- TH.runIO (examplesFrom "test/examples.toml")
--                  let Just (CsvExample _ employees _) =
--                        find (\(CsvExample k _ _) -> k == "employees") csvExamples
--                  generateCode "EmployeeRec" employees)

-- | To generate example generated code from raw CSV data, add the csv
-- to @examples.toml@ and set the @generated@ key to an empty
-- string. Then load this file into a REPL that has @:set
-- -XTemplateHaskell@ and evaluate @putStrLn $(manualGeneration
-- "employees")@, for example.
--
-- Note that to load this file into a REPL may require some fiddling
-- with the path to the examples file in the 'csvTests' splice above.
manualGeneration :: String -> Q Exp
manualGeneration k = do csvExamples <- TH.runIO (examplesFrom "test/examples.toml")
                        maybe (error ("Table " ++ k ++ " not found"))
                              (generateCode "Row")
                              (getFirst $ foldMap aux csvExamples)
  where aux :: CsvExample -> First String
        aux (CsvExample k' c _) = if k == k' then pure c else mempty

type ManagersRow =
  Record ["id" :-> Int, "manager" :-> Text, "age" :-> Int, "pay" :-> Double]

type NoTruncateRow = Record ["id" :-> Int, "foo" :-> Int]

newtype Code = Code String
instance Show Code where show (Code x) = x
instance Eq Code where
  Code a == Code b = clean a == clean b
    where clean = go (removePrefix moduleNames) . filter (not . isSpace)
          go _ [] = []
          go f s@(c:cs) = case removePrefix moduleNames s of
                            Nothing -> c : go f cs
                            Just n -> go f (drop n s)
          moduleNames = [ "GHC.Maybe." ]
          removePrefix [] _ = Nothing
          removePrefix (w:ws) s | w `isPrefixOf` s = Just (length w)
                                | otherwise = removePrefix ws s

shouldBeWithinEpsilon :: Double -> Double -> Expectation
shouldBeWithinEpsilon actual expected =
  unless (abs (actual - expected) < 1e-6)
         (assertFailure
           (show actual
            ++ " is not very close to the expected value "
            ++ show expected))

main :: IO ()
main = do
  hspec $
    do
#if __GLASGOW_HASKELL__ >= 804
       describe "Haskell type generation" $
         mapM_ (\(CsvExample k _ g, g') -> it k (Code g' `shouldBe` Code g)) csvTests
#endif
       -- describe "Multiple tables" $
       --    do _g <- H.runIO $
       --             generatedFrom "test/examples.toml" "managers_employees"
       --       it "Shouldn't duplicate columns" pending
       describe "Writing CSV Data" $ do
         let csvInput = case find ((== "managers") . name . fst) csvTests of
                          Just (ex,_) -> csv ex
                          Nothing -> error "Couldn't find managers test data"
         frame <- H.runIO $
                  withSystemTempFile "FramesSpecOutput" $ \fp h -> do
                    hClose h
                    writeFile fp csvInput
                    inCoreAoS (readTable fp) :: IO (Frame ManagersRow)
         let Identity csvOutput = toListM (produceCSV frame)
         frame2 <- H.runIO $
                   withSystemTempFile "FramesSpecOutput" $ \fp h -> do
                     hClose h
                     writeFile fp (unlines csvOutput)
                     inCoreAoS (readTable fp) :: IO (Frame ManagersRow)

         -- The test data isn't formatted quite how we'd do it: text
         -- fields aren't quoted, and salaries represented as Doubles
         -- do not have decimal points.
         let csvInput' = T.replace "Joe" "\"Joe\""
                       . T.replace "Sarah" "\"Sarah\""
                       . T.replace "\"80,000\"" "80000.0"
                       $ T.pack csvInput
         it "Produces expected output" $
           T.unlines (map T.pack csvOutput) `shouldBe` csvInput'
         it "Produces parseable output" $
           unless (frame == frame2)
                  (assertFailure "Reparsed CSV differs from input")
       describe "Latin1 Text Encoding" $
         do managers <- H.runIO (Latin.managers)
            it "Parses" $
              managers `shouldBe` ["João", "Esperança"]
       describe "Skip Missing Data" $ do
         let csvInput = case find ((== "NoTruncate") . name . fst) csvTests of
                          Just (ex,_) -> csv ex
                          Nothing -> error "Couldn't find NoTruncate test data"
         frameMissing <- H.runIO $
                         withSystemTempFile "FramesSpecOutput" $ \fp h -> do
                           hClose h
                           writeFile fp csvInput
                           inCoreAoS (readTable fp) :: IO (Frame NoTruncateRow)
         it "Doesn't truncate after missing data" $
           F.length frameMissing `shouldBe` 3
       describe "Skip NA Data" $ do
         let csvInput = case find ((== "NoTruncateNA") . name . fst) csvTests of
                          Just (ex,_) -> csv ex
                          Nothing -> error "Couldn't find NoTruncateNA test data"
         frameMissing <- H.runIO $
                         withSystemTempFile "FramesSpecOutput" $ \fp h -> do
                           hClose h
                           writeFile fp csvInput
                           inCoreAoS (readTable fp) :: IO (Frame NoTruncateRow)
         it "Doesn't truncate after missing data" $
           F.length frameMissing `shouldBe` 4
       describe "Parses Issue 114 Data" $ do
         fnames <- H.runIO Issue114.testNames
         it "Extracts facility_name" $
           fnames `shouldBe` ["LILLIAN B. SMITH, ET AL", "MUSSER, B W \"B\""]
       describe "Supports user-supplied column names" $ do
         res0 <- H.runIO (NoHeader.getJobAndSchooling 0)
         it "Extracts job from row 0" $
           fst <$> res0 `shouldBe` Just "gov.administrators"
         it "Extracts schooling from row 0" $
           maybe 99 snd res0 `shouldBeWithinEpsilon` 13.11
         res9 <- H.runIO (NoHeader.getJobAndSchooling 9)
         it "Extracts job from row 9" $
           fst <$> res9 `shouldBe` Just "mining.engineers"
         it "Extracts schooling from row 9" $
           maybe 99 snd res9 `shouldBeWithinEpsilon` 14.64
         resWithHeader <- H.runIO UncurryFold.averageRatio
         resNoHeader <- H.runIO UncurryFoldNoHeader.averageRatio
         it "Produces identical numerics independent of column names" $
           resWithHeader `shouldBe` resNoHeader
       describe "Can read into Maybe values" $ do
         (n,avg) <- H.runIO UncurryFoldPartialData.incomeOfUnknownPrestige
         it "Found the expected number of partial rows" $
           n `shouldBe` 4
         it "Computed the average income correctly" $
           avg `shouldBe` 3344.5
       describe "Can generate categorical types" $ do
         mSmall <- H.runIO Categorical.fifthMonthSmall
         it "Generates enumerated types for small cardinality sets" $
           mSmall `shouldBe` Just Categorical.SmallMonthMay
         mLarge <- H.runIO Categorical.fifthMonthLarge
         it "Falls back to Text when the number of variants grows" $
           mLarge `shouldBe` Just (T.pack "May")
         mCustom <- H.runIO Categorical.fifthMonthCustom
         it "Can parse into manually-specified categorical variables" $
           mCustom `shouldBe` Just Categorical.MyMay
       describe "Detects parse failures" $ do
         caught <- H.runIO $
           (runSafeT $ do
             _ <- readColHeaders @Columns
                    defaultParser
                    (produceTokens "test/data/multiline.csv" defaultSep)
             return False)
            `catch` \(_ :: ErrorCall) -> return True
         it "Fails on embedded newlines" caught
       describe "Chunking" $ do
         let everyTenthEducation = [13.11,12.39,15.97,12.79,12.09,11.13,8.5,7.64,8.78,6.92,10.0]
         inCoreChunks <- H.runIO Chunks.chunkInCore
         it "Can split in-memory data into chunks" $
           inCoreChunks `shouldBe` everyTenthEducation
         streamedChunks <- H.runIO Chunks.chunkStream
         it "Can split an input stream into Frame chunks" $
           streamedChunks `shouldBe` everyTenthEducation
