{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module LatinTest where

import           Data.Vinyl    (Rec)
import           Frames        ((:->), MonadSafe, Text, runSafeEffect, rget)
import           Frames.CSV    (declareColumn, pipeTableMaybe, readFileLatin1Ln)
import           Frames.Rec
import           Pipes         (Producer, (>->))
import qualified Pipes.Prelude as P

declareColumn "mId" ''Int
declareColumn "manager" '' Text
declareColumn "age" ''Int
declareColumn "pay" ''Int

type ManColumns = '["id" :-> Int, "manager" :-> Text, "age" :-> Int, "pay" :-> Text]
type ManRow = Record ManColumns
type ManMaybe = Rec Maybe ManColumns

manStreamM :: MonadSafe m => Producer ManMaybe m ()
manStreamM = readFileLatin1Ln "test/data/latinManagers.csv" >-> pipeTableMaybe

managers :: IO [Text]
managers =
  runSafeEffect . P.toListM $
  manStreamM >-> P.map recMaybe >-> P.concat >-> P.map (rget manager)
