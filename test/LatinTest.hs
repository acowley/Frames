{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module LatinTest where

import           Data.Vinyl    (Rec)
import           Frames
import           Frames.CSV    (declareColumn, pipeTableMaybe, readFileLatin1Ln)
import           Pipes         (Producer, (>->))
import qualified Pipes.Prelude as P

declareColumn "mId" ''Int
declareColumn "manager" '' Text
declareColumn "age" ''Int
declareColumn "pay" ''Int

type ManColumns = '["id" :-> Int, "manager" :-> Text, "age" :-> Int, "pay" :-> Text]
type ManRow = Record ManColumns
type ManMaybe = Rec (Maybe :. ElField) ManColumns

manStreamM :: MonadSafe m => Producer ManMaybe m ()
manStreamM = readFileLatin1Ln "test/data/latinManagers.csv" >-> pipeTableMaybe

managers :: IO [Text]
managers =
  runSafeEffect . P.toListM $
  manStreamM >-> P.map recMaybe >-> P.concat >-> P.map (rgetField @Manager)
