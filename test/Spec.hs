{-# LANGUAGE CPP, DataKinds, OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeOperators, FlexibleContexts, TypeFamilies #-}
module Main (manualGeneration, main) where
import Control.Exception (ErrorCall, catch)
import Control.Monad (unless, mzero)
import Data.Functor.Identity
import Data.Char
import qualified Data.Char as C
import qualified Data.Foldable as F
import Data.List (find, isPrefixOf)
import Data.Monoid (First(..))
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vinyl (RPureConstrained, RecApplicative)
import Data.Vinyl.CoRec (foldCoRec, FoldRec)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Frames
import Frames.ColumnTypeable (Parsed(..), Parseable)
import Frames.ColumnUniverse (bestRep)
import Frames.CSV (produceCSV)
import Frames.CSV (defaultParser, produceTokens, defaultSep, readColHeaders)
import Frames.InCore (VectorFor)
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

inferType
  :: forall ts.
    ( RPureConstrained Parseable ts
    , FoldRec ts ts
    , RecApplicative ts
    , T.Text ∈ ts
    )
  => Proxy ts -> [Text] -> Parsed Type
inferType _ inputs =
  foldCoRec parsedTypeRep (foldMap (bestRep @ts) inputs)

inferTypeCommon, inferTypeCustom :: [Text] -> Parsed Type
inferTypeCommon = inferType (Proxy @CommonColumns)
inferTypeCustom = inferType (Proxy @MyColumns)

isInferredType :: String -> Parsed Type -> Bool
isInferredType typeName (Definitely (ConT typeName'))
  = typeName' == mkName typeName
isInferredType _ _ = False

typeIsUncertain :: Parsed Type -> Bool
typeIsUncertain (Possibly _) = True
typeIsUncertain (Definitely _) = False

-- Custom data type, as defined in demo/TutorialZipCode.hs
data ZipT = ZipUS Int Int Int Int Int
          | ZipWorld Char Char Char Char Char
  deriving (Eq, Ord, Show)

type instance VectorFor ZipT = V.Vector

instance Readable ZipT  where
  fromText t
      | T.length t == 5 = let cs@[v,w,x,y,z] = T.unpack t
                          in if all C.isDigit cs
                             then let [a,b,c,d,e] = map C.digitToInt cs
                               in pure $ ZipUS a b c d e
                             else return $ ZipWorld v w x y z
      | otherwise = mzero

instance Parseable ZipT where

type MyColumns = ZipT ': CommonColumns

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
         -- let csvInput' = T.replace "Joe" "\"Joe\""
         --               . T.replace "Sarah" "\"Sarah\""
         --               . T.replace "\"80,000\"" "80000.0"
         --               $ T.pack csvInput
         let csvInput' = T.replace "\"80,000\"" "80000.0" (T.pack csvInput)
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

       describe "Column type inference" $ do
         it "Selects Bool to represent boolean-like values" $ do
           inferTypeCommon ["1"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["0"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["T"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["F"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["True"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["False"] `shouldSatisfy` isInferredType "Bool"
         it "Selects Int to r(Proxy @CommonColumns) epresent integer-like values" $ do
           inferTypeCommon ["2"] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon ["3"] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon ["1337"] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon ["-1"] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon [T.pack $ show (maxBound :: Int)] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon [T.pack $ show (minBound :: Int)] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon [T.pack $ show ((maxBound :: Int) + 1)] `shouldSatisfy` isInferredType "Int"  -- hmm... should this be Integer?
           inferTypeCommon ["0.0"] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon ["1.0"] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon ["2.0"] `shouldSatisfy` isInferredType "Int"
           inferTypeCommon ["-1.0"] `shouldSatisfy` isInferredType "Int"
         it "Selects Double to represent double-like values" $ do
           inferTypeCommon ["0.000001"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["1.1"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["2.000001"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["1337.1337"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["-1.1"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["-1337.0003"] `shouldSatisfy` isInferredType "Double"
         it "Selects Text otherwise" $ do
           inferTypeCommon ["foo"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["-"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["."] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["Here be some data"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["7.7.7"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["7.0-"] `shouldSatisfy` isInferredType "Text"
         it "Is inconclusive about empty data" $
           foldCoRec parsedTypeRep (bestRep @CommonColumns "") `shouldBe` Possibly (ConT $ mkName "Text")

         it "Selects Bool when multiple boolean-like values are seen" $ do
           inferTypeCommon ["0", "1"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["1", "0"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["1", "0", "0", "1"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["T", "F"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCommon ["T", "F", "0", "1"] `shouldSatisfy` isInferredType "Bool"
         it "Selects Int over Bool when values include those beyond 0 and 1" $ do
           inferTypeCommon ["1", "2"]`shouldSatisfy` isInferredType  "Int"
           inferTypeCommon ["2", "1"]`shouldSatisfy` isInferredType  "Int"
           inferTypeCommon ["2", "1", "0"]`shouldSatisfy` isInferredType  "Int"
           inferTypeCommon ["2.0", "1.0", "0.0"]`shouldSatisfy` isInferredType  "Int"
         it "Selects Double over Int when at least one decimal is seen" $ do
           inferTypeCommon ["1.1", "2", "3"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["1.0", "2", "3.1"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["1", "2", "3.1"] `shouldSatisfy` isInferredType "Double"
           inferTypeCommon ["1", "2", "3.00001", ""] `shouldSatisfy` isInferredType "Double" -- hmm... should this be 'Maybe Double'?
         it "Falls back on Text when at least one non-numeric is seen" $ do
           inferTypeCommon ["foo"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["1", "foo"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["True", "foo"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["1", "2", "foo"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["1", "2", "3.00001", "foo"] `shouldSatisfy` isInferredType "Text"
           inferTypeCommon ["1", "2", "", "3.00001", "foo"] `shouldSatisfy` isInferredType "Text"

         it "Makes the same inferences of common data types after a custom type is added" $ do
           inferTypeCustom ["1"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCustom ["False"] `shouldSatisfy` typeIsUncertain -- Uncertain because it looks like a 'world' zipcode
           inferTypeCustom ["False", "True"] `shouldSatisfy` typeIsUncertain
           inferTypeCustom ["False", "True", "True"] `shouldSatisfy` typeIsUncertain
           inferTypeCustom ["False", "True", "True", "False"] `shouldSatisfy` typeIsUncertain -- At this point surely it should decide it's Bool? But it's not currently that smart.
           inferTypeCustom ["F"] `shouldSatisfy` isInferredType "Bool"
           inferTypeCustom ["3"] `shouldSatisfy` isInferredType "Int"
           inferTypeCustom ["-1.0"] `shouldSatisfy` isInferredType "Int"
           inferTypeCustom ["0.000001"] `shouldSatisfy` isInferredType "Double"
           inferTypeCustom ["-1337.0003"] `shouldSatisfy` isInferredType "Double"
           inferTypeCustom ["foo"] `shouldSatisfy` isInferredType "Text"
           inferTypeCustom ["Here be some data"] `shouldSatisfy` isInferredType "Text"
           inferTypeCustom ["7.0-"] `shouldSatisfy` isInferredType "Text"
           foldCoRec parsedTypeRep (bestRep @MyColumns "") `shouldBe` Possibly (ConT $ mkName "Text")

         it "Infers a ZipT where appropriate" $ do
           inferTypeCustom ["12345"] `shouldSatisfy` typeIsUncertain -- May be Int, ZipT, or Text
           inferTypeCustom ["12345", "abcde"] `shouldSatisfy` typeIsUncertain
           inferTypeCustom ["12345", "abcde", "54321"] `shouldSatisfy` typeIsUncertain
           -- inferTypeCustom ["abcde"] `shouldSatisfy` isInferredType "ZipT" -- TODO: this is still inferring `Possibly (ConT Text)`. It should pick the more specific `ZipT` instead.
