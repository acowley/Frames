{-# LANGUAGE OverloadedStrings #-}
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Network.HTTP.Client

getFLinsurance :: IO ()
getFLinsurance = withManager defaultManagerSettings $ \m -> 
                 httpLbs req m  >>=
                   B.writeFile "data/FL2.csv"
                 . B.map fixup . fromEntry . fromJust
                 . findEntryByPath "FL_insurance_sample.csv"
                 . toArchive
                 . responseBody
  where fixup '\r' = '\n'
        fixup c = c
        Just req = parseUrl "http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip"

getAdultIncome :: IO ()
getAdultIncome = withManager defaultManagerSettings $ \m ->
                 httpLbs req m >>=
                   B.writeFile "data/adult.csv"
                 . B.append colNames
                 . responseBody
  where Just req = parseUrl "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
        colNames = "age, workclass, fnlwgt, education, education-num, \
                   \marital-status, occupation, relationship, race, sex, \
                   \capital-gain, capital-loss, hours-per-week, \
                   \native-country\n"

main :: IO ()
main = do getFLinsurance
          getAdultIncome
