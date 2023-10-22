{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- | Parse data including dates times in some implicit local time zone
-- into an absolute time using a supplied time zone.
module Main where
import Data.Proxy (Proxy(Proxy))
import Frames
import Frames.CSV
import Frames.TH (RowGen(columnUniverse), colQ, rowGen)
import Columns
import Pipes (Producer, (>->), runEffect)
import qualified Pipes.Prelude as P
import Pipes.Safe
import Frames (ColumnUniverse)
import Columns (MyColumns)

-- tableTypes' rowGen { columnUniverse = $(colQ ''MyColumns) } "/Users/acowley/Projects/Frames/demo/TimeZones/users.csv"
tableTypes' ((rowGen "demo/TimeZones/users.csv") { columnUniverse = Proxy @MyColumns })

loadUsers :: Producer Row (SafeT IO) ()
loadUsers = readTable "demo/TimeZones/users.csv"

main :: IO ()
main = runSafeEffect $ loadUsers >-> P.print
