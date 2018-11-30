{-# LANGUAGE OverloadedStrings #-}
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Network.HTTP.Client
import System.Directory (createDirectoryIfMissing)

getPrestige :: IO ()
getPrestige = do m <- newManager defaultManagerSettings
                 httpLbs req m >>=
                   B.writeFile "data/prestige.csv" . responseBody
  where Just req = parseUrlThrow "http://vincentarelbundock.github.io/Rdatasets/csv/car/Prestige.csv"

getFLinsurance :: IO ()
getFLinsurance = do m <- newManager defaultManagerSettings
                    httpLbs req m >>=
                        B.writeFile "data/FL2.csv"
                      . B.map fixup . fromEntry . fromJust
                      . findEntryByPath "FL_insurance_sample.csv"
                      . toArchive
                      . responseBody
  where fixup '\r' = '\n'
        fixup c = c
        Just req = parseUrlThrow "http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip"

getAdultIncome :: IO ()
getAdultIncome = do m <- newManager defaultManagerSettings
                    httpLbs req m >>=
                        B.writeFile "data/adult.csv"
                      . B.append colNames
                      . B.unlines
                      . map (\ln -> maybe ln id
                                    . getFirst
                                    $ foldMap (First . ($ ln))
                                        [ B.stripSuffix ", <=50K"
                                        , B.stripSuffix ", >50K" ])
                      . B.lines
                      . responseBody
  where Just req = parseUrlThrow "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
        colNames = "age, workclass, fnlwgt, education, education-num, \
                   \marital-status, occupation, relationship, race, sex, \
                   \capital-gain, capital-loss, hours-per-week, \
                   \native-country\n"

main :: IO ()
main = do createDirectoryIfMissing False "data"
          getPrestige
          getFLinsurance
          getAdultIncome
