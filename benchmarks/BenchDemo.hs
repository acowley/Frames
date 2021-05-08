{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell, TypeApplications #-}
-- | Demonstration of streaming data processing. Try building with
-- cabal (@cabal build benchdemo@), then running in bash with
-- something like,
--
-- @$ /usr/bin/time -l dist/build/benchdemo/benchdemo 2>&1 | head -n 4@
-- Or, for cabal-install 3 (using Linux and GHC-8.8.2 as an example),
-- @$ time dist-newstyle/build/x86_64-linux/ghc-8.8.2/Frames-0.6.2/x/benchdemo/build/benchdemo/benchdemo@
-- Or, for all resource usage on linux (avoid the bash builtin time),
-- @$ $(which time) -v dist-newstyle/build/x86_64-linux/ghc-8.8.2/Frames-0.6.2/x/benchdemo/build/benchdemo/benchdemo@
import qualified Control.Foldl as F
import Frames
import Frames.TH (rowGen, RowGen (rowTypeName, inferencePrefix))
import Pipes.Prelude (fold)

-- The simple use of 'tableTypes' commented out here is what one
-- typically uses; it infers column types based the first 1000 rows of
-- the data file. In this data file, however, we need to see more rows
-- to properly identify the types.

-- tableTypes "Ins" "data/FL2.csv"
tableTypes' (rowGen "data/FL2.csv") { rowTypeName = "Ins", inferencePrefix = 2500 }

main :: IO ()
main = do (lat,lng,n) <- runSafeT $ F.purely fold f (readTable "data/FL2.csv")
          print $ lat / n
          print $ lng / n
  where f :: F.Fold Ins (Double,Double,Double)
        f = (,,) <$> F.handles pointLatitude F.sum
                 <*> F.handles pointLongitude F.sum
                 <*> F.genericLength
