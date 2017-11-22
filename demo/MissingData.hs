{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, QuasiQuotes, TemplateHaskell,
             TypeOperators, UndecidableInstances #-}
-- | An example of dealing with rows that contain missing data. We may
-- want to fill in the gaps with default values.
import Data.Monoid ((<>), First(..))
import Data.Vinyl (Rec(..), rmap, RecApplicative, rapply)
import Data.Vinyl.Functor (Lift(..))
import Frames hiding ((:&))
import Pipes (cat, Producer, (>->))
import Pipes.Prelude as P

-- An en passant Default class
class Default a where
  def :: a

type MyInt = "int" :-> Int
type MyString = "string" :-> String
type MyBool = "bool" :-> Bool

-- Note that we define instances for column types. This lets us have
-- different defaults for different column names.
instance Default MyInt where def = Col 0
instance Default MyString where def = Col ""
instance Default MyBool where def = Col False

-- We can write instances for /all/ 'Rec' values.
instance (Applicative f, AllConstrained Default ts, RecApplicative ts)
  => Default (Rec f ts) where
  def = reifyDict [pr|Default|] (pure def)

-- Just to try it out at the 'Identity' functor.
defRec :: Record '[MyString, MyInt, MyBool]
defRec = def

-- A default record at a more interesting 'Functor'.
defFirst :: Rec First '[MyString, MyInt, MyBool]
defFirst = def

-- Real data often has holes. Here we have the 'MyString' column, but
-- not the others.
holyRow :: Rec First '[MyString, MyInt, MyBool]
holyRow = rmap First $ pure (Col "joe") :& Nothing :& Nothing :& RNil

-- We can fill in the holes with our default record.
unholy :: Maybe (Record '[MyString, MyInt, MyBool])
unholy = recMaybe . rmap getFirst $ rapply (rmap (Lift . flip (<>)) def) holyRow

-- * Reading a CSV file with missing data

instance Default ("col_a" :-> Int) where def = Col 0
instance Default ("col_b" :-> Text) where def = Col mempty

tableTypes "Row" "data/missing.csv"

-- | Fill in missing columns with a default 'Row' value synthesized
-- from 'Default' instances.
holesFilled :: MonadSafe m => Producer Row m ()
holesFilled = readTableMaybe "data/missing.csv" >-> P.map (fromJust . holeFiller)
  where holeFiller :: Rec Maybe (RecordColumns Row) -> Maybe Row
        holeFiller = recMaybe
                   . rmap getFirst
                   . rapply (rmap (Lift . flip (<>)) def)
                   . rmap First
        fromJust = maybe (error "Frames holesFilled failure") id

showFilledHoles :: IO ()
showFilledHoles = runSafeT (pipePreview holesFilled 10 cat)

main :: IO ()
main = return ()
