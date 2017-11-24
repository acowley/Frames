{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module PipesSafe where

import           Data.Vinyl    (Rec)
import           Frames        ((:->), MonadSafe, Text, runSafeEffect)
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

printManagers :: IO ()
printManagers =
  runSafeEffect $ manStreamM >-> P.map recMaybe >-> P.concat >-> P.print
