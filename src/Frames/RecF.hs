{-# LANGUAGE AllowAmbiguousTypes,
             ConstraintKinds,
             CPP,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             MultiParamTypeClasses,
             PatternSynonyms,
             RankNTypes,
             ScopedTypeVariables,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             ViewPatterns #-}
module Frames.RecF (V.rappend, V.rtraverse, rdel, CanDelete,
                    frameCons, frameConsA, frameSnoc,
                    pattern (:&), pattern Nil, AllCols,
                    UnColumn, AsVinyl(..), mapMono, mapMethod,
                    runcurry, runcurry', runcurryA, runcurryA',
                    ShowRec, showRec, ColFun, ColumnHeaders,
                    columnHeaders, reifyDict) where
import Data.List (intercalate)
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl (Rec(RNil), RecApplicative(rpure))
import qualified Data.Vinyl.Curry as V
import Data.Vinyl.Functor (Compose, Identity)
import Data.Vinyl.TypeLevel
import Frames.Col
import Frames.TypeLevel
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Add a column to the head of a row.
frameCons :: Functor f => f a -> V.Rec f rs -> V.Rec f (s :-> a ': rs)
frameCons = (V.:&) . fmap Col
{-# INLINE frameCons #-}

-- | Add a pure column to the head of a row.
frameConsA :: Applicative f => a -> V.Rec f rs -> V.Rec f (s :-> a ': rs)
frameConsA = (V.:&) . fmap Col . pure
{-# INLINE frameConsA #-}

-- | Separate the first element of a row from the rest of the row.
frameUncons :: Functor f => V.Rec f (s :-> r ': rs) -> (f r, V.Rec f rs)
frameUncons (x V.:& xs) = (fmap getCol x, xs)
{-# INLINE frameUncons #-}

-- | Add a column to the tail of a row. Note that the supplied value
-- should be a 'Col' to work with the @Frames@ tooling.
frameSnoc :: V.Rec f rs -> f r -> V.Rec f (rs ++ '[r])
frameSnoc r x = V.rappend r (x V.:& RNil)
{-# INLINE frameSnoc #-}

pattern Nil :: Rec f '[]
pattern Nil <- V.RNil where
  Nil = V.RNil

pattern (:&) :: Functor f => f r -> Rec f rs -> Rec f (s :-> r ': rs)
pattern x :& xs <- (frameUncons -> (x, xs)) where
  x :& xs = frameCons x xs

class ColumnHeaders (cs::[*]) where
  -- | Return the column names for a record.
  columnHeaders :: proxy (Rec f cs) -> [String]

instance ColumnHeaders '[] where
  columnHeaders _ = []

instance forall cs s c. (ColumnHeaders cs, KnownSymbol s)
    => ColumnHeaders (s :-> c ': cs) where
  columnHeaders _ = symbolVal (Proxy::Proxy s) : columnHeaders (Proxy::Proxy (Rec f cs))

-- | A type function to convert a 'Record' to a 'Rec'. @ColFun f (Rec
-- rs) = Rec f rs@.
type family ColFun f x where
  ColFun f (Rec Identity rs) = Rec f rs

-- | Strip the column information from each element of a list of
-- types.
type family UnColumn ts where
  UnColumn '[] = '[]
  UnColumn ((s :-> t) ': ts) = t ': UnColumn ts

-- | Enforce a constraint on the payload type of each column.
type AllCols c ts = AllConstrained c (UnColumn ts)

-- | Remove the column name phantom types from a record, leaving you
-- with an unadorned Vinyl 'V.Rec'.
class AsVinyl ts where
  toVinyl :: Functor f => Rec f ts -> V.Rec f (UnColumn ts)
  fromVinyl :: Functor f => V.Rec f (UnColumn ts) -> Rec f ts

instance AsVinyl '[] where
  toVinyl _ = V.RNil
  fromVinyl _ = V.RNil

instance AsVinyl ts => AsVinyl (s :-> t ': ts) where
  toVinyl (x V.:& xs) = fmap getCol x V.:& toVinyl xs
  fromVinyl (x V.:& xs) = fmap Col x V.:& fromVinyl xs
#if __GLASGOW_HASKELL__ < 800
  fromVinyl _ = error "GHC coverage checker isn't great"
#endif

-- | Map a function across a homogeneous, monomorphic 'V.Rec'.
mapMonoV :: (Functor f, AllAre a ts)
         => (a -> b) -> V.Rec f ts -> V.Rec f (ReplaceAll b ts)
mapMonoV _ V.RNil = V.RNil
mapMonoV f (x V.:& xs) = fmap f x V.:& mapMonoV f xs

-- | Map a function across a homogeneous, monomorphic 'Rec'.
mapMono :: (AllAre a (UnColumn ts), AsVinyl ts, Functor f,
            AsVinyl (ReplaceColumns b ts),
            ReplaceAll b (UnColumn ts) ~ UnColumn (ReplaceColumns b ts))
        => (a -> b) -> Rec f ts -> Rec f (ReplaceColumns b ts)
mapMono f = fromVinyl . mapMonoV f . toVinyl

-- | Map a typeclass method across a 'V.Rec' each of whose fields
-- have instances of the typeclass.
mapMethodV :: forall c f ts. (Functor f, AllConstrained c ts)
           => (forall a. c a => a -> a) -> V.Rec f ts -> V.Rec f ts
mapMethodV f = go
  where go :: AllConstrained c ts' => V.Rec f ts' -> V.Rec f ts'
        go V.RNil = V.RNil
        go (x V.:& xs) = fmap f x V.:& go xs

-- | Map a typeclass method across a 'Rec' each of whose fields
-- has an instance of the typeclass.
mapMethod :: forall c f ts.
             (Functor f, AllConstrained c (UnColumn ts), AsVinyl ts)
          => (forall a. c a => a -> a) -> Rec f ts -> Rec f ts
mapMethod f = fromVinyl . mapMethodV @c f . toVinyl

-- * Currying Adapted from "Vinyl.Curry"

-- | N-ary version of 'uncurry' over functorial frame rows. See 'V.runcurry'.
runcurry :: (Functor f, AsVinyl ts)
         => V.CurriedF f (UnColumn ts) a -> Rec f ts -> a
runcurry = (. toVinyl) . V.runcurry
{-# INLINABLE runcurry #-}

-- | N-ary version of 'uncurry' over pure frame rows. See 'V.runcurry''.
runcurry' :: AsVinyl ts => V.Curried (UnColumn ts) a -> Rec Identity ts -> a
runcurry' = (. toVinyl) . V.runcurry'
{-# INLINABLE runcurry' #-}

-- | Lift an N-ary function to work over a row of 'Applicative'
-- computations. See 'V.runcurryA'.
runcurryA' :: (Applicative f, AsVinyl ts)
           => V.Curried (UnColumn ts) a -> Rec f ts -> f a
runcurryA' = (. toVinyl) . V.runcurryA'

-- | Lift an N-ary function over types in @g@ to work over a record of
-- 'Compose'd 'Applicative' computations. A more general version of
-- 'runcurryA''.
runcurryA :: (Applicative f, Functor g, AsVinyl ts)
          => V.CurriedF g (UnColumn ts) a -> Rec (Compose f g) ts -> f a
runcurryA = (. toVinyl) . V.runcurryA

-- | A constraint that a field can be deleted from a record.
type CanDelete r rs = (V.RElem r rs (RIndex r rs), RDelete r rs V.⊆ rs)

-- | Delete a field from a record
rdel :: CanDelete r rs => proxy r -> Rec f rs -> Rec f (RDelete r rs)
rdel _ = V.rcast

-- | The ability to pretty print a 'Rec''s fields.
class Functor f => ShowRec f rs where
  showRec' :: Rec f rs -> [String]

instance Functor f => ShowRec f '[] where
  showRec' _ = []

instance forall s f a rs. (KnownSymbol s, Show (f (Col' s a)), ShowRec f rs)
  => ShowRec f (s :-> a ': rs) where
  showRec' (x :& xs) = show (col' <$> x :: f (Col' s a)) : showRec' xs
  showRec' _ = error "GHC coverage error"

-- | Pretty printing of 'Rec' values.
showRec :: ShowRec f rs => Rec f rs -> String
showRec r = "{" ++ intercalate ", " (showRec' r) ++ "}"

-- | Build a record whose elements are derived solely from a
-- constraint satisfied by each.
reifyDict :: forall c f proxy ts. (AllConstrained c ts, RecApplicative ts)
          => proxy c -> (forall a. c a => f a) -> Rec f ts
reifyDict _ f = go (rpure Nothing)
  where go :: AllConstrained c ts' => Rec Maybe ts' -> Rec f ts'
        go RNil = RNil
        go (_ V.:& xs) = f V.:& go xs
