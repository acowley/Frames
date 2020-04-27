{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Issue145 where

import Frames.TH (RowGen (..), rowGenCat, tableTypes')

tableTypes' (rowGenCat "test/data/issue145.csv") { rowTypeName = "Row", tablePrefix = "C" }
