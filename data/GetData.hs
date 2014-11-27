import Codec.Archive.Zip
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Network.HTTP.Client

main :: IO ()
main = withManager defaultManagerSettings $ \m -> 
         httpLbs req m  >>=
           B.writeFile "data/FL2.csv"
           . B.map fixup . fromEntry . fromJust
           . findEntryByPath "FL_insurance_sample.csv"
           . toArchive
           . responseBody
  where fixup '\r' = '\n'
        fixup c = c
        Just req = parseUrl "http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip"
