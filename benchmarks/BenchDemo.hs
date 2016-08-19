{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell #-}
-- | Demonstration of streaming data processing. Try building with
-- cabal (@cabal build benchdemo@), then running in bash with
-- something like,
--
-- @$ /usr/bin/time -l dist/build/benchdemo/benchdemo 2>&1 | head -n 4@
import qualified Control.Foldl as F
import Frames
import Pipes.Prelude (fold)

tableTypes "Ins" "data/FL2.csv"

main :: IO ()
main = do (lat,lng,n) <- F.purely fold f (readTable "data/FL2.csv")
          print $ lat / n
          print $ lng / n
  where f :: F.Fold Ins (Double,Double,Double)
        f = (,,) <$> F.handles pointLatitude F.sum
                 <*> F.handles pointLongitude F.sum
                 <*> F.genericLength
