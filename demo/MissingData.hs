{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, OverloadedStrings, PolyKinds,
             QuasiQuotes, RankNTypes, TemplateHaskell,
             TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
-- | An example of dealing with rows that contain missing data. We may
-- want to fill in the gaps with default values.
import Data.Monoid (First(..))
import Data.Vinyl (Rec(..), ElField(..), rapply, xrec, rmapX)
import Data.Vinyl.Functor (Compose(..), (:.))
import Data.Vinyl.Class.Method

import Frames
import Frames.TH (declarePrefixedColumnType)

import Pipes (cat, Producer, (>->))
import Pipes.Prelude as P

-- An en passant Default class
class Default a where
  def :: a

type MyInt = "int" :-> Int
type MyString = "string" :-> String
type MyBool = "bool" :-> Bool

instance Default (ElField MyInt) where def = Field 0
instance Default (ElField MyString) where def = Field ""
instance Default (ElField MyBool) where def = Field False

instance (Applicative f, Default a) => Default (f a) where def = pure def
instance Default (f (g a)) => Default (Compose f g a) where def = Compose def

instance RecPointed Default f ts => Default (Rec f ts) where
  def = rpointMethod @Default def

-- Just to try it out with a plain 'Record'.
defRec :: Record '[MyString, MyInt, MyBool]
defRec = def

-- A default record at a more interesting 'Functor'.
defFirst :: Rec (First :. ElField) '[MyString, MyInt, MyBool]
defFirst = def

-- Real data often has holes. Here we have the 'MyString' column, but
-- not the others.
holyRow :: Rec (First :. ElField) '[MyString, MyInt, MyBool]
holyRow = xrec (pure "joe", mempty, mempty)

test :: Rec (First :. ElField) '[ "name" :-> String, "age" :-> Int ]
test = xrec (pure "joe", mempty)

-- We can fill in the holes with our default record.
unholy :: Maybe (Record '[MyString, MyInt, MyBool])
unholy = recMaybe . rmapX @(First :. ElField) getFirst
       $ rapply (rmapX @(First :. ElField) (flip mappend) def) holyRow


-- * Reading a CSV file with missing data

instance Default (ElField ("col_a" :-> Int)) where def = Field 0
instance Default (ElField ("col_b" :-> Text)) where def = Field mempty

tableTypes "Row" "data/missing.csv"

-- | Fill in missing columns with a default 'Row' value synthesized
-- from 'Default' instances.
holesFilled :: MonadSafe m => Producer Row m ()
holesFilled = readTableMaybe "data/missing.csv" >-> P.map (fromJust . holeFiller)
  where holeFiller :: Rec (Maybe :. ElField) (RecordColumns Row) -> Maybe Row
        holeFiller = recMaybe . rmapX @(First :. ElField) getFirst
                   . rapply (rmapX @(First :. ElField) (flip mappend) def)
                   . rmapX @_ @(First :. ElField) First
        fromJust = maybe (error "Frames holesFilled failure") id

showFilledHoles :: IO ()
showFilledHoles = runSafeT (pipePreview holesFilled 10 cat)

-- Perhaps we want to parse possibly missing data in a particular
-- column. Here, @col_a@ may or may not have an integer, while @col_b@
-- definitely has a text field. The row type itself, @RowMaybeA@ is
-- /not/ interpreted as a @Maybe :. ElField@ (the composition of
-- 'Maybe' and 'ElField').

declarePrefixedColumnType "col_a" "withHoles" [t|Maybe Int|]
declarePrefixedColumnType "col_b" "withHoles" [t|Text|]

type RowMaybeA = Rec ElField '["col_a" :-> Maybe Int, "col_b" :-> Text]

holesIncluded :: MonadSafe m => Producer RowMaybeA m ()
holesIncluded = readTable "data/missing.csv"

showWithHoles :: IO ()
showWithHoles = runSafeT (pipePreview holesIncluded 10 cat)

main :: IO ()
main = return ()
