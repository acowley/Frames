{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, MultiParamTypeClasses, PolyKinds,
             ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
module Frames.Melt where
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.CoRec (CoRec(..))
import Data.Vinyl.TypeLevel
import Frames.Col
import Frames.Frame (Frame(..), FrameRec)
import Frames.Rec
import Frames.RecF (ColumnHeaders(..))

type family Elem t ts :: Bool where
  Elem t '[] = 'False
  Elem t (t ': ts) = 'True
  Elem t (s ': ts) = Elem t ts

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True b = 'True
  Or a b = b

type family Not a :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family Disjoint ss ts :: Bool where
  Disjoint '[] ts = 'True
  Disjoint (s ': ss) ts = Or (Not (Elem s ts)) (Disjoint ss ts)

type ElemOf ts r = RElem r ts (RIndex r ts)

class RowToColumn ts rs where
  rowToColumnAux :: Proxy ts -> Rec f rs -> [CoRec f ts]

instance RowToColumn ts '[] where
  rowToColumnAux _ _ = []

instance (r ∈ ts, RowToColumn ts rs) => RowToColumn ts (r ': rs) where
  rowToColumnAux p (x :& xs) = CoRec x : rowToColumnAux p xs

-- | Transform a record into a list of its fields, retaining proof
-- that each field is part of the whole.
rowToColumn :: RowToColumn ts ts => Rec f ts -> [CoRec f ts]
rowToColumn = rowToColumnAux Proxy

meltAux :: forall vs ss ts.
           (vs ⊆ ts, ss ⊆ ts, Disjoint ss ts ~ 'True, ts ≅ (vs ++ ss),
           ColumnHeaders vs, RowToColumn vs vs)
        => Record ts
        -> [Record ("value" :-> CoRec ElField vs ': ss)]
meltAux r = map (\val -> Field val :& ids) (rowToColumn vals)
  where ids = rcast r :: Record ss
        vals = rcast r :: Record vs

type family RDeleteAll ss ts where
  RDeleteAll '[] ts = ts
  RDeleteAll (s ': ss) ts = RDeleteAll ss (RDelete s ts)

transform :: forall rs as bs . (as ⊆ rs, RDeleteAll as rs ⊆ rs)
             => (Record as -> Record bs) -> Record rs -> Record (RDeleteAll as rs ++ bs)
transform f xs = rcast @(RDeleteAll as rs) xs `rappend` f (rcast xs)

-- | Rename a column
retypeColumn :: forall x y rs . ( V.KnownField x
                                , V.KnownField y
                                , V.Snd x ~ V.Snd y
                                , ElemOf rs x
                                , RDelete x rs ⊆ rs)
                => Record rs -> Record (RDelete x rs V.++ '[y])
retypeColumn = transform @rs @'[x] @'[y] (\r -> (rgetField @x r &: V.RNil))

-- | This is 'melt', but the variables are at the front of the record,
-- which reads a bit odd.
meltRow' :: forall proxy vs ts ss. (vs ⊆ ts, ss ⊆ ts, vs ~ RDeleteAll ss ts,
            Disjoint ss ts ~ 'True, ts ≅ (vs ++ ss),
            ColumnHeaders vs, RowToColumn vs vs)
         => proxy ss
         -> Record ts
         -> [Record ("value" :-> CoRec ElField vs ': ss)]
meltRow' _ = meltAux

-- | Turn a cons into a snoc after the fact.
retroSnoc :: forall t ts. Record (t ': ts) -> Record (ts ++ '[t])
retroSnoc (x :& xs) = go xs
  where go :: Record ss -> Record (ss ++ '[t])
        go RNil = x :& RNil
        go (y :& ys) = y :& go ys

-- | Like @melt@ in the @reshape2@ package for the @R@ language. It
-- stacks multiple columns into a single column over multiple
-- rows. Takes a specification of the id columns that remain
-- unchanged. The remaining columns will be stacked.
--
-- Suppose we have a record, @r :: Record [Name,Age,Weight]@. If we
-- apply @melt [pr1|Name|] r@, we get two values with type @Record
-- [Name, "value" :-> CoRec Identity [Age,Weight]]@. The first will
-- contain @Age@ in the @value@ column, and the second will contain
-- @Weight@ in the @value@ column.
meltRow :: (vs ⊆ ts, ss ⊆ ts, vs ~ RDeleteAll ss ts,
            Disjoint ss ts ~ 'True, ts ≅ (vs ++ ss),
            ColumnHeaders vs, RowToColumn vs vs)
        => proxy ss
        -> Record ts
        -> [Record (ss ++ '["value" :-> CoRec ElField vs])]
meltRow = (map retroSnoc .) . meltRow'

class HasLength (ts :: [k]) where
  hasLength :: proxy ts -> Int

instance HasLength '[] where hasLength _ = 0
instance forall t ts. HasLength ts => HasLength (t ': ts) where
  hasLength _ = 1 + hasLength (Proxy :: Proxy ts)

-- | Applies 'meltRow' to each row of a 'FrameRec'.
melt :: forall vs ts ss proxy.
        (vs ⊆ ts, ss ⊆ ts, vs ~ RDeleteAll ss ts, HasLength vs,
         Disjoint ss ts ~ 'True, ts ≅ (vs ++ ss),
         ColumnHeaders vs, RowToColumn vs vs)
     => proxy ss
     -> FrameRec ts
     -> FrameRec (ss ++ '["value" :-> CoRec ElField vs])
melt p (Frame n v) = Frame (n*numVs) go
  where numVs = hasLength (Proxy :: Proxy vs)
        go i = let (j,k) = i `quotRem` numVs
               in meltRow p (v j) !! k
