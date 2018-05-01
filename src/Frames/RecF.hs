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
             PolyKinds,
             RankNTypes,
             ScopedTypeVariables,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             ViewPatterns #-}
module Frames.RecF (V.rappend, V.rtraverse, rdel, CanDelete,
                    -- frameCons, frameConsA, frameSnoc,
                    -- pattern (:&), pattern Nil,
                    AllCols,
                    UnColumn, -- AsVinyl(..)
                    mapMono,
                    -- mapMethod,
                    -- runcurry, runcurry', runcurryA, runcurryA',
                    -- ShowRec,
                    -- showRec,
                    ColFun, ColumnHeaders,
                    columnHeaders, reifyDict) where
-- import Data.List (intercalate)
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl (Rec(RNil), RecApplicative(rpure))
import Data.Vinyl.Functor ((:.))
import Data.Vinyl.TypeLevel
import Frames.Col
import Frames.TypeLevel
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

-- -- | Add a column to the head of a row.
-- frameCons :: (Functor f, KnownSymbol s)
--           => f a -> Rec f rs -> Rec f (s :-> a ': rs)
-- frameCons = (V.:&) . fmap Col
-- {-# INLINE frameCons #-}

-- -- | Add a pure column to the head of a row.
-- frameConsA :: (Applicative f, KnownSymbol s)
--            => a -> Rec f rs -> Rec f (s :-> a ': rs)
-- frameConsA = (V.:&) . fmap Col . pure
-- {-# INLINE frameConsA #-}

-- -- | Separate the first element of a row from the rest of the row.
-- frameUncons :: Functor f => Rec f (s :-> r ': rs) -> (f r, Rec f rs)
-- frameUncons (x V.:& xs) = (fmap getCol x, xs)
-- {-# INLINE frameUncons #-}

-- -- | Add a column to the tail of a row. Note that the supplied value
-- -- should be a 'Col' to work with the @Frames@ tooling.
-- frameSnoc :: Rec f rs -> f r -> Rec f (rs ++ '[r])
-- frameSnoc r x = V.rappend r (x V.:& RNil)
-- {-# INLINE frameSnoc #-}

-- pattern Nil :: Rec f '[]
-- pattern Nil <- RNil where
--   Nil = RNil

-- pattern (:&) :: (Functor f, KnownSymbol s) => f r -> Rec f rs -> Rec f (s :-> r ': rs)
-- pattern x :& xs <- (frameUncons -> (x, xs)) where
--   x :& xs = frameCons x xs

class ColumnHeaders (cs::[(Symbol,*)]) where
  -- | Return the column names for a record.
  columnHeaders :: proxy (Rec f cs) -> [String]

instance ColumnHeaders '[] where
  columnHeaders _ = []

instance forall cs s c. (ColumnHeaders cs, KnownSymbol s)
    => ColumnHeaders (s :-> c ': cs) where
  columnHeaders _ = symbolVal (Proxy::Proxy s) : columnHeaders (Proxy::Proxy (Rec f cs))

-- | A type function to convert a 'Rec f' to a 'Rec (g :. f)'. @ColFun
-- f (Record rs) = Rec (f :. ElField) rs@.
type family ColFun f x where
  ColFun f (Rec g rs) = Rec (f :. g) rs

-- | Strip the column information from each element of a list of
-- types.
type family UnColumn (ts :: [(Symbol,*)]) where
  UnColumn '[] = '[]
  UnColumn ((s :-> t) ': ts) = t ': UnColumn ts

-- | Enforce a constraint on the payload type of each column.
type AllCols c ts = AllConstrained c (UnColumn ts)

-- | Remove the column name phantom types from a record, leaving you
-- with an unadorned Vinyl 'Rec'.
-- class AsVinyl f (ts :: [k]) where
--   toVinyl :: Rec f ts -> Rec f (UnColumn ts)
--   fromVinyl :: Rec f (UnColumn ts) -> Rec f ts

-- instance AsVinyl '[] where
--   toVinyl _ = RNil
--   fromVinyl _ = RNil

-- instance (AsVinyl ts, KnownSymbol s) => AsVinyl (s :-> t ': ts) where
--   toVinyl (x V.:& xs) = fmap getCol x V.:& toVinyl xs
--   fromVinyl (x V.:& xs) = fmap Col x V.:& fromVinyl xs
-- #if __GLASGOW_HASKELL__ < 800
--   fromVinyl _ = error "GHC coverage checker isn't great"
-- #endif

-- | Map a function across a homogeneous, monomorphic 'Rec'.
mapMono :: (Functor f, AllAre a ts)
        => (a -> b) -> Rec f ts -> Rec f (ReplaceAll b ts)
mapMono _ RNil = RNil
mapMono f (x V.:& xs) = fmap f x V.:& mapMono f xs

-- | Map a function across a homogeneous, monomorphic 'Rec'.
-- mapMono :: (AllAre a (UnColumn ts), AsVinyl ts, Functor f,
--             AsVinyl (ReplaceColumns b ts),
--             ReplaceAll b (UnColumn ts) ~ UnColumn (ReplaceColumns b ts))
--         => (a -> b) -> Rec f ts -> Rec f (ReplaceColumns b ts)
-- mapMono f = fromVinyl . mapMonoV f . toVinyl

-- | Map a typeclass method across a 'Rec' each of whose fields
-- have instances of the typeclass.
-- mapMethodV :: forall c f ts. (Functor f, AllConstrained c ts)
--            => (forall a. c a => a -> a) -> Rec f ts -> Rec f ts
-- -- mapMethodV f = go
-- --   where go :: AllConstrained c ts' => Rec f ts' -> Rec f ts'
-- --         go RNil = RNil
-- --         go (x V.:& xs) = fmap f x V.:& go xs
-- mapMethodV = V.rmapMethod . fmap

-- | Map a typeclass method across a 'Rec' each of whose fields
-- has an instance of the typeclass.
-- mapMethod :: forall c f ts.
--              (Functor f, AllConstrained c (UnColumn ts), AsVinyl ts)
--           => (forall a. c a => a -> a) -> Rec f ts -> Rec f ts
-- mapMethod f = fromVinyl . mapMethodV @c f . toVinyl

-- * Currying Adapted from "Vinyl.Curry"

-- -- | N-ary version of 'uncurry' over functorial frame rows. See 'V.runcurry'.
-- runcurry :: (Functor f)
--          => V.CurriedF f (UnColumn ts) a -> Rec f ts -> a
-- runcurry = (. toVinyl) . V.runcurry
-- {-# INLINABLE runcurry #-}

-- -- | N-ary version of 'uncurry' over pure frame rows. See 'V.runcurry''.
-- runcurry' :: AsVinyl ts => V.Curried (UnColumn ts) a -> FieldRec ts -> a
-- runcurry' = (. toVinyl) . V.runcurry'
-- {-# INLINABLE runcurry' #-}

-- -- | Lift an N-ary function to work over a row of 'Applicative'
-- -- computations. See 'V.runcurryA'.
-- runcurryA' :: (Applicative f, AsVinyl ts)
--            => V.Curried (UnColumn ts) a -> Rec f ts -> f a
-- runcurryA' = (. toVinyl) . V.runcurryA'

-- -- | Lift an N-ary function over types in @g@ to work over a record of
-- -- 'Compose'd 'Applicative' computations. A more general version of
-- -- 'runcurryA''.
-- runcurryA :: (Applicative f, Functor g, AsVinyl ts)
--           => V.CurriedF g (UnColumn ts) a -> Rec (Compose f g) ts -> f a
-- runcurryA = (. toVinyl) . V.runcurryA

-- | A constraint that a field can be deleted from a record.
type CanDelete r rs = (V.RElem r rs (RIndex r rs), RDelete r rs V.⊆ rs)

-- | Delete a field from a record
rdel :: CanDelete r rs => proxy r -> Rec f rs -> Rec f (RDelete r rs)
rdel _ = V.rcast

-- -- | The ability to pretty print a 'Rec''s fields.
-- class ShowRec f (rs :: [k]) where
--   showRec' :: Rec f rs -> [String]

-- instance Functor f => ShowRec f '[] where
--   showRec' _ = []

-- instance forall s f a (rs :: [(Symbol,*)]). (KnownSymbol s, Show (f (Col' s a)), ShowRec f rs)
--   => ShowRec f (s :-> a ': rs) where
--   showRec' (x :& xs) = show (col' <$> x :: f (Col' s a)) : showRec' xs
--   showRec' _ = error "GHC coverage error"

-- | Pretty printing of 'Rec' values.
-- showRec :: ShowRec f rs => Rec f rs -> String
-- showRec :: (Show (Rec f rs), Functor f) => Rec f rs -> String
-- showRec r = show (V.rmap (fmap Col') r)

-- | Build a record whose elements are derived solely from a
-- constraint satisfied by each.
reifyDict :: forall c f ts. (AllConstrained c ts, RecApplicative ts)
          => (forall a. c a => f a) -> Rec f ts
reifyDict f = go (rpure Proxy)
  where go :: AllConstrained c ts' => Rec Proxy ts' -> Rec f ts'
        go RNil = RNil
        go (_ V.:& xs) = f V.:& go xs
