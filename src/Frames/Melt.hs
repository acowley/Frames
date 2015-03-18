{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables,
             TypeFamilies, TypeOperators, UndecidableInstances #-}
module Frames.Melt where
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..))
import Data.Vinyl.TypeLevel
import Frames.Col
import Frames.CoRec (CoRec)
import qualified Frames.CoRec as C
import Frames.Exploration
import Frames.Rec
import Frames.RecF (ColumnHeaders(..), frameCons)
import Frames.TypeLevel

type family Elem t ts :: Bool where
  Elem t '[] = False
  Elem t (t ': ts) = True
  Elem t (s ': ts) = Elem t ts

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or True b = True
  Or a b = b

type family Not a :: Bool where
  Not True = False
  Not False = True

type family Disjoint ss ts :: Bool where
  Disjoint '[] ts = True
  Disjoint (s ': ss) ts = Or (Not (Elem s ts)) (Disjoint ss ts)

type ElemOf ts r = RElem r ts (RIndex r ts)

class RowToColumn ts rs where
  rowToColumnAux :: Proxy ts -> Rec f rs -> [CoRec f ts]

instance RowToColumn ts '[] where
  rowToColumnAux _ _ = []

instance (r ∈ ts, RowToColumn ts rs) => RowToColumn ts (r ': rs) where
  rowToColumnAux p (x :& xs) = C.Col x : rowToColumnAux p xs

-- | Transform a record into a list of its fields, retaining proof
-- that each field is part of the whole.
rowToColumn :: RowToColumn ts ts => Rec f ts -> [CoRec f ts]
rowToColumn = rowToColumnAux Proxy

melt :: forall vs ss ts.
        (vs ⊆ ts, ss ⊆ ts, Disjoint ss ts ~ True, ts ≅ (vs ++ ss),
         ColumnHeaders vs, RowToColumn vs vs)
     => Record ts
     -> [Record ("variable" :-> String ': "value" :-> CoRec Identity vs ': ss)]
melt r = zipWith (\name val -> frameCons (Identity name) $
                               frameCons (Identity val) ids)
                 (columnHeaders (Just vals)) (rowToColumn vals)
  where ids = rcast r :: Record ss
        vals = rcast r :: Record vs

type Age = "age" :-> Int
type Weight = "weight" :-> Double
type Name = "name" :-> String

testRec :: Record ["name" :-> String, "age" :-> Int, "weight" :-> Double]
testRec = frameCons (Identity "bob")
        $ frameCons (Identity 23)
        $ frameCons (Identity 75.2) RNil

type family RDeleteAll ss ts where
  RDeleteAll '[] ts = ts
  RDeleteAll (s ': ss) ts = RDeleteAll ss (RDelete s ts)

-- | This is 'melt', but the variables are at the front of the record,
-- which reads a bit odd.
melt' :: forall proxy vs ts ss. (vs ⊆ ts, ss ⊆ ts, vs ~ RDeleteAll ss ts,
         Disjoint ss ts ~ True, ts ≅ (vs ++ ss),
         ColumnHeaders vs, RowToColumn vs vs)
      => proxy ss
      -> Record ts
      -> [Record ("value" :-> CoRec Identity vs ': ss)]
melt' _ = meltAux

-- | Turn a cons into a snoc after the fact.
retroSnoc :: forall t ts. Record (t ': ts) -> Record (ts ++ '[t])
retroSnoc (x :& xs) = go xs
  where go :: Record ss -> Record (ss ++ '[t])
        go RNil = x :& RNil
        go (y :& ys) = y :& go ys

melt :: (vs ⊆ ts, ss ⊆ ts, vs ~ RDeleteAll ss ts,
         Disjoint ss ts ~ True, ts ≅ (vs ++ ss),
         ColumnHeaders vs, RowToColumn vs vs)
     => proxy ss
     -> Record ts
     -> [Record (ss ++ '["value" :-> CoRec Identity vs])]
melt = (map retroSnoc .) . melt'
