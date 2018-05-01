{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, QuasiQuotes, RankNTypes,
             TemplateHaskell, TypeApplications, TypeFamilies,
             TypeOperators, UndecidableInstances #-}
-- | An example of dealing with rows that contain missing data. We may
-- want to fill in the gaps with default values.
import Data.Monoid (First(..))
import Data.Vinyl (Rec(..), ElField(..), rmap, rapply)
import Data.Vinyl (XRec(..), IsoXRec(..))
import Data.Vinyl.Functor (Compose(..), Lift(..), (:.))
import Data.Vinyl.Class.Method
import Frames

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
holyRow = fromXRec $ pure ("joe") ::& mempty ::& mempty ::& XRNil

liftCompose2 :: (f (g a) -> h (j b) -> k (l c))
             -> Compose f g a -> Compose h j b -> Compose k l c
liftCompose2 f (Compose x) (Compose y) = Compose (f x y)

mappendC :: Monoid (f (g x)) => Compose f g x -> Compose f g x -> Compose f g x
mappendC = liftCompose2 mappend

-- We can fill in the holes with our default record.
unholy :: Maybe (Record '[MyString, MyInt, MyBool])
unholy = recMaybe . rmap (Compose . getFirst . getCompose)
       $ rapply (rmap (Lift . flip mappendC) def) holyRow

-- * Reading a CSV file with missing data

instance Default (ElField ("col_a" :-> Int)) where def = Field 0
instance Default (ElField ("col_b" :-> Text)) where def = Field mempty

tableTypes "Row" "data/missing.csv"

recAla :: (forall a. f a -> g a) -> (forall a. g a -> f a) -> (Rec g rs -> Rec g rs) -> Rec f rs -> Rec f rs
recAla tin tout f = rmap tout . f . rmap tin

alaC :: (forall (t :: u). g t -> h t) -> (g :. f) a -> (h :. f) a
alaC f (Compose x) = Compose (f x)

-- | Fill in missing columns with a default 'Row' value synthesized
-- from 'Default' instances.
holesFilled :: MonadSafe m => Producer Row m ()
holesFilled = readTableMaybe "data/missing.csv" >-> P.map (fromJust . holeFiller)
  where holeFiller :: Rec (Maybe :. ElField) (RecordColumns Row) -> Maybe Row
        holeFiller = recMaybe . recAla (alaC First) (alaC getFirst)
                                       (rapply (rmap (Lift . flip mappendC) def))
        fromJust = maybe (error "Frames holesFilled failure") id

showFilledHoles :: IO ()
showFilledHoles = runSafeT (pipePreview holesFilled 10 cat)

main :: IO ()
main = return ()
