{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DataCSV where

import Data.Bifunctor (first)
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH.Syntax (Lift (..))
import qualified Toml
import Validation

data CsvExample = CsvExample
    { name :: String
    , csv :: String
    , generated :: String
    }
    deriving (Lift, Show)

keyText :: Toml.Key -> T.Text
keyText = F.fold . cleanup . map Toml.unPiece . F.toList . Toml.unKey
  where
    -- Top-level table names are parsed as @"name" :|
    -- ["name"]@. Remove that duplication here.
    cleanup [x, y] | x == y = [x]
    cleanup x = x

-- | Parse a TOML file that is a top-level table whose values are all
-- the same type. The @tomland@ codec API is centered around starting
-- with a key, but a top-level table does not have a key, so we must
-- use the lower level 'Toml.parse' and 'Toml.tomlTables' before
-- repeatedly applying the provided 'Toml.TomlCodec'.
parseFileOf :: forall a. Toml.TomlCodec a -> T.Text -> Either [T.Text] [(T.Text, a)]
parseFileOf codec =
    first (map Toml.prettyTomlDecodeError)
        . validationToEither
        . traverse (uncurry go)
        . Toml.toList
        . Toml.tomlTables
        . either (error . show) id
        . Toml.parse
  where
    go :: Toml.Key -> Toml.TOML -> Validation [Toml.TomlDecodeError] (T.Text, a)
    go k v = (keyText k,) <$> Toml.runTomlCodec codec v

parseExamples :: FilePath -> IO (Either [T.Text] [CsvExample])
parseExamples = fmap (fmap (map mkExample) . parseFileOf exampleCodec) . T.readFile
  where
    exampleCodec = Toml.pair (Toml.string "csv") (Toml.string "generated")
    mkExample (name', (csv', generated')) =
        CsvExample (T.unpack name') csv' generated'

-- | Wraps 'parseExamples' to call 'error' on any parse errors.
examplesFrom :: FilePath -> IO [CsvExample]
examplesFrom = fmap (either (error . show) id) . parseExamples
