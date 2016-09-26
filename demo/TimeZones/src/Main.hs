{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Parse data including dates times in some implicit local time zone
-- into an absolute time using a supplied time zone.
module Main where
import Frames
import Frames.CSV
import Columns
import Pipes (Producer, (>->), runEffect)
import qualified Pipes.Prelude as P

tableTypes' rowGen { columnUniverse = $(colQ ''MyColumns) } "users.csv"

loadUsers :: Producer Row IO ()
loadUsers = readTable "users.csv"

main :: IO ()
main = runEffect $ loadUsers >-> P.print
