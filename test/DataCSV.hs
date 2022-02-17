{-# LANGUAGE DeriveLift, OverloadedStrings, TemplateHaskell #-}
module DataCSV where
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as H
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.TH.Syntax (Lift(..))
import Text.Toml
import Text.Toml.Types (Node (VTable, VString), Table)

data CsvExample = CsvExample {
  name :: String
  , csv :: String
  , generated :: String }
  deriving Lift

-- instance Lift CsvExample where
--   lift (CsvExample n c g) = [e| CsvExample n c g |]

examplesFrom :: FilePath -> IO [CsvExample]
examplesFrom fp =
  (either error id
   . ((first show . parseTomlDoc "examples") >=> go)
   . decodeUtf8)
  <$> BS.readFile fp
  where go :: Table -> Either String [CsvExample]
        go = fmap catMaybes . mapM (uncurry ex . first T.unpack) . H.toList
        ex :: String -> Node -> Either String (Maybe CsvExample)
        ex k (VTable v) =
          do c <- case H.lookup "csv" v of
                    Nothing -> Right Nothing -- ("No csv key in "++k)
                    Just (VString c) -> Right (Just (T.unpack c))
                    Just _ -> Left ("csv key not a string in " ++ k)
             g <- case H.lookup "generated" v of
                    Nothing -> Left ("No generated key in " ++ k)
                    Just (VString g) -> Right (Just (T.unpack g))
                    Just _ -> Left ("generated key not a string in " ++ k)
             return (CsvExample k <$> c <*> g)
        ex k _ = Left (k ++ " is not a table")

generatedFrom :: FilePath -> String -> IO String
generatedFrom fp key = (either error id . (>>= go)
                        . first show . parseTomlDoc "examples")
                       <$> T.readFile fp
  where go :: Table -> Either String String
        go toml = do tbl <- case H.lookup (T.pack key) toml of
                              Just (VTable t) -> Right t
                              _ -> Left (key ++ " is not a table")
                     case H.lookup "generated" tbl of
                       Just (VString g) -> Right (T.unpack g)
                       Just _ -> Left ("generated key not a string in " ++ key)
                       Nothing -> Left ("No generated key in " ++ key)
