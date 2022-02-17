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
                    StripFieldNames(..),
                    mapMono,
                    -- mapMethod,
                    -- runcurry, runcurry', runcurryA, runcurryA',
                    -- ShowRec,
                    -- showRec,
                    ColFun, ColumnHeaders,
                    columnHeaders) where
-- import Data.List (intercalate)
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl (Rec(..))
import Data.Vinyl.Derived (StripFieldNames(..), Unlabeled)
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

class ColumnHeaders (cs::[(Symbol, Type)]) where
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
type family UnColumn (ts :: [(Symbol, Type)]) where
  UnColumn '[] = '[]
  UnColumn ((s :-> t) ': ts) = t ': UnColumn ts

-- | Enforce a constraint on the payload type of each column.
type AllCols c ts = AllConstrained c (UnColumn ts)

-- | Map a function across a 'Rec' with monomorphic (i.e. all the
-- same) indices.
mapMonoV :: (AllAre a ts, Functor f)
         => (a -> b)
         -> Rec f ts
         -> Rec f (ReplaceAll b ts)
mapMonoV _ RNil = RNil
mapMonoV f (x :& xs) = fmap f x :& mapMonoV f xs

-- | Map a function across a homogeneous 'Rec' of named values. This
-- is a thin wrapper over 'mapMonoV'.
mapMono :: (AllAre a (Unlabeled ts),
            StripFieldNames ts,
            StripFieldNames (ReplaceAllSnd b ts),
            ReplaceAll b (Unlabeled ts) ~ Unlabeled (ReplaceAllSnd b ts))
        => (a -> b)
        -> Rec V.ElField ts
        -> Rec V.ElField (ReplaceAllSnd b ts)
mapMono f = withNames . mapMonoV f . stripNames

-- | A constraint that a field can be deleted from a record.
type CanDelete r rs = (V.RElem r rs (RIndex r rs), RDelete r rs V.⊆ rs)

-- | Delete a field from a record
rdel :: CanDelete r rs => Rec f rs -> Rec f (RDelete r rs)
rdel = V.rcast
