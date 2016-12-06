{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell #-}
module Main (main) where
import Frames

-- These data files have overlapping column definitions. Frames should
-- not try to re-define an existing identifier.

tableTypes "ManagerRec" "test/data/managers.csv"
tableTypes "EmployeeRec" "test/data/employees.csv"

main :: IO ()
main = return ()
