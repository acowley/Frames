{-# LANGUAGE ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             MultiParamTypeClasses,
             PatternSynonyms,
             RankNTypes,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators,
             ViewPatterns #-}
module Frames.RecF (V.rappend, V.rtraverse, rdel, CanDelete,
                    frameCons, pattern (:&), pattern Nil, AllCols,
                    UnColumn, AsVinyl(..), mapMono, mapMethod,
                    ShowRec, showRec, ColFun, ColumnHeaders, 
                    columnHeaders, reifyDict) where
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl (Rec(RNil), RecApplicative(rpure))
import Data.Vinyl.Functor (Identity)
import Data.Vinyl.TypeLevel
import Frames.Col
import Frames.TypeLevel
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Add a column to the head of a row.
frameCons :: Functor f => f a -> V.Rec f rs -> V.Rec f (s :-> a ': rs)
frameCons = (V.:&) . fmap Col
{-# INLINE frameCons #-}

-- | Separate the first element of a row from the rest of the row.
frameUncons :: Functor f => V.Rec f (s :-> r ': rs) -> (f r, V.Rec f rs)
frameUncons (x V.:& xs) = (fmap getCol x, xs)
{-# INLINE frameUncons #-}

pattern Nil = V.RNil
pattern x :& xs <- (frameUncons -> (x, xs))

-- NOTE: A bidirectional pattern synonym would be great, but we'll
-- have to wait for GHC 7.10 to gain wide acceptance before depending
-- upon its availability.

class ColumnHeaders (cs::[*]) where
  -- | Return the column names for a record.
  columnHeaders :: proxy (Rec f cs) -> [String]

instance ColumnHeaders '[] where
  columnHeaders _ = []

instance forall cs s c. (ColumnHeaders cs, KnownSymbol s)
    => ColumnHeaders (s :-> c ': cs) where
  columnHeaders _ = symbolVal (Proxy::Proxy s) : columnHeaders (Proxy::Proxy (Rec f cs))

-- | A type function to convert a 'Rec' to a 'Rec'. @ColFun f (Rec
-- rs) = Rec f rs@.
type family ColFun f x where
  ColFun f (Rec Identity rs) = Rec f rs

-- | Strip the column information from each element of a list of
-- types.
type family UnColumn ts where
  UnColumn '[] = '[]
  UnColumn ((s :-> t) ': ts) = t ': UnColumn ts

-- | Enforce a constraint on the payload type of each column.
type AllCols c ts = LAll c (UnColumn ts)

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
  fromVinyl _ = error "GHC coverage checker isn't great"

-- | Map a function across a homogeneous, monomorphic 'V.Rec'.
mapMonoV :: (Functor f, AllAre a ts) => (a -> a) -> V.Rec f ts -> V.Rec f ts
mapMonoV _ V.RNil = V.RNil
mapMonoV f (x V.:& xs) = fmap f x V.:& mapMonoV f xs

-- | Map a function across a homogeneous, monomorphic 'Rec'.
mapMono :: (AllAre a (UnColumn ts), Functor f, AsVinyl ts)
        => (a -> a) -> Rec f ts -> Rec f ts
mapMono f = fromVinyl . mapMonoV f . toVinyl

-- | Map a typeclass method across a 'V.Rec' each of whose fields
-- have instances of the typeclass.
mapMethodV :: forall c f ts. (Functor f, LAll c ts)
           => Proxy c -> (forall a. c a => a -> a) -> V.Rec f ts -> V.Rec f ts
mapMethodV _ f = go
  where go :: LAll c ts' => V.Rec f ts' -> V.Rec f ts'
        go V.RNil = V.RNil
        go (x V.:& xs) = fmap f x V.:& go xs

-- | Map a typeclass method across a 'Rec' each of whose fields
-- has an instance of the typeclass.
mapMethod :: forall f c ts. (Functor f, LAll c (UnColumn ts), AsVinyl ts)
          => Proxy c -> (forall a. c a => a -> a) -> Rec f ts -> Rec f ts
mapMethod p f = fromVinyl . mapMethodV p f . toVinyl

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
reifyDict :: forall c f proxy ts. (LAll c ts, RecApplicative ts)
          => proxy c -> (forall a. c a => f a) -> Rec f ts
reifyDict _ f = go (rpure Nothing)
  where go :: LAll c ts' => Rec Maybe ts' -> Rec f ts'
        go RNil = RNil
        go (_ V.:& xs) = f V.:& go xs
