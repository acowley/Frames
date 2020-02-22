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
import Pipes.Prelude (fold)

tableTypes "Ins" "data/FL2.csv"

main :: IO ()
main = do (lat,lng,n) <- runSafeT $ F.purely fold f (readTable "data/FL2.csv")
          print $ lat / n
          print $ lng / n
  where f :: F.Fold Ins (Double,Double,Double)
        f = (,,) <$> F.handles pointLatitude F.sum
                 <*> F.handles pointLongitude F.sum
                 <*> F.genericLength
