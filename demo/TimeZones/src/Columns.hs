{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Define the column types used to represent our data. Here, we wish
-- to parse data captured as 'Data.Time.LocalTime.LocalTime' values
-- into the \"America/Chicago\" time zone.
module Columns (MyColumns, TimeIn(..), Chicago(..)) where
import Frames (CommonColumns)
import Frames.ColumnTypeable (Parseable(..))
import TimeIn

-- | Define a 'Parseable' instance for @TimeIn "America/Chicago"@
timeIn "America/Chicago"

-- | We need this newtype because Template Haskell can not handle the
-- type @TimeIn "America/Chicago"@ as of @GHC-8.0.1@ and
-- @template-haskell-2.11.0.0@
newtype Chicago = Chicago (TimeIn "America/Chicago") deriving Show

instance Parseable Chicago where
  parse = fmap (fmap Chicago) . parse

-- | The column types we expect our data to conform to
type MyColumns = Chicago ': CommonColumns
