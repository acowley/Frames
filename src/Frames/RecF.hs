{-# LANGUAGE DataKinds,
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
module Frames.RecF (RecF, V.rappend, V.rtraverse, RDel, rdel,
                    frameCons, pattern (:&), pattern Nil,
                    ShowRecF, showRecF, ColFun) where
import Control.Applicative
import Data.List (intercalate)
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl.Functor (Identity)
import Data.Vinyl.TypeLevel
import Frames.Col
import Frames.TypeLevel
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | A record parameterized by a functor. This can be useful for
-- distributing an effect across a record. For instance, a per-element
-- validation can be pulled out to act as a per-record validation.
type RecF = V.Rec

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

class ColumnNames (cs::[*]) where
  -- | Return the column names for a record.
  columnNames :: proxy (RecF f cs) -> [String]

instance ColumnNames '[] where
  columnNames _ = []

instance forall cs s c. (ColumnNames cs, KnownSymbol s)
    => ColumnNames (s :-> c ': cs) where
  columnNames _ = symbolVal (Proxy::Proxy s) : columnNames (Proxy::Proxy (RecF f cs))

-- | A type function to convert a 'Rec' to a 'RecF'. @ColFun f (Rec
-- rs) = RecF f rs@.
type family ColFun f x where
  ColFun f (RecF Identity rs) = RecF f rs

-- | Delete a field from a record
rdel :: forall f r rs i proxy. (RDel r rs i, RIndex r rs ~ i)
     => proxy r -> RecF f rs -> RecF f (RDelete r rs)
rdel p = rdel' p (Proxy :: Proxy i)

-- | The ability to delete a field from a record depends upon the
-- original record having the chosen field.
class RDel r (rs :: [*]) (i :: Nat) where
  rdel' :: proxy r -> proxy' i -> V.Rec f rs -> V.Rec f (RDelete r rs)

instance RDel r (r ': rs) Z where
  rdel' _ _ (_ V.:& xs) = xs

instance forall r s rs i.
         (RIndex r (s ': rs) ~ S i,
          RDel r rs i,
          RDelete r (s ': rs) ~ (s ': RDelete r rs))
  => RDel r (s ': rs) (S i) where
  rdel' p _ (x V.:& xs) = x V.:& (rdel' p (Proxy::Proxy i) xs)

-- | The ability to pretty print a 'RecF''s fields.
class Functor f => ShowRecF f rs where
  showRecF' :: RecF f rs -> [String]

instance Functor f => ShowRecF f '[] where
  showRecF' _ = []

instance forall s f a rs. (KnownSymbol s, Show (f (Col' s a)), ShowRecF f rs)
  => ShowRecF f (s :-> a ': rs) where
  showRecF' (x :& xs) = show (col' <$> x :: f (Col' s a)) : showRecF' xs
  showRecF' _ = error "GHC coverage error"

-- | Pretty printing of 'RecF' values.
showRecF :: ShowRecF f rs => RecF f rs -> String
showRecF r = "{" ++ intercalate ", " (showRecF' r) ++ "}"
