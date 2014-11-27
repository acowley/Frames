{-# LANGUAGE DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators #-}
module Frames.RecF (RecF(..), rappend, rtraverse, RDel, rdel,
                    ShowRecF, showRecF, ColFun) where
import Control.Applicative
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Monoid
import Data.Proxy
import Frames.Col
import Frames.TypeLevel
import GHC.TypeLits (KnownSymbol)

-- | A record parameterized by a functor. This can be useful for
-- distributing an effect across a record. For instance, a per-element
-- validation can be pulled out to act as a per-record validation.
data RecF :: (* -> *) -> [*] -> * where
  Nil :: RecF f '[]
  (:&) :: !(f a) -> !(RecF f cs) -> RecF f (s :-> a ': cs)
infixr 5 :&

-- | A type function to convert a 'Rec' to a 'RecF'. @ColFun f (Rec
-- rs) = RecF f rs@.
type family ColFun f x where
  ColFun f (RecF Identity rs) = RecF f rs

-- | Record append.
rappend :: RecF f xs -> RecF f ys -> RecF f (xs ++ ys)
rappend Nil ys = ys
rappend (x :& xs) ys = x :& rappend xs ys

-- | Pull a context that has been distributed over a record out to the
-- head of the record.
rtraverse :: Applicative h
          => (forall x. f x -> h (g x))
          -> RecF f cs
          -> h (RecF g cs)
rtraverse _ Nil = pure Nil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs
{-# INLINABLE rtraverse #-}

-- | Delete a field from a record
rdel :: forall f r rs i proxy. (RDel r rs i, RIndex r rs ~ i)
     => proxy r -> RecF f rs -> RecF f (RDelete r rs)
rdel p = rdel' p (Proxy :: Proxy i)

-- | The ability to delete a field from a record depends upon the
-- original record having the chosen field.
class RDel r (rs :: [*]) (i :: Peano) where
  rdel' :: proxy r -> proxy' i -> RecF f rs -> RecF f (RDelete r rs)

instance RDel r (r ': rs) Z where
  rdel' _ _ (_ :& xs) = xs

instance forall r s rs i.
         (RIndex r (s ': rs) ~ S i,
          RDel r rs i,
          RDelete r (s ': rs) ~ (s ': RDelete r rs))
  => RDel r (s ': rs) (S i) where
  rdel' p _ (x :& xs) = x :& (rdel' p (Proxy::Proxy i) xs)

-- | The ability to pretty print a 'RecF''s fields.
class Functor f => ShowRecF f rs where
  showRecF' :: RecF f rs -> [String]

instance Functor f => ShowRecF f '[] where showRecF' Nil = []

instance forall s f a rs. (KnownSymbol s, Show (f (Col' s a)), ShowRecF f rs)
  => ShowRecF f (s :-> a ': rs) where
  showRecF' (x :& xs) = show (col' <$> x :: f (Col' s a)) : showRecF' xs

-- | Pretty printing of 'RecF' values.
showRecF :: ShowRecF f rs => RecF f rs -> String
showRecF r = "{" ++ intercalate ", " (showRecF' r) ++ "}"

instance Monoid (RecF f '[]) where
  mempty = Nil
  mappend _ _ = Nil

instance (Monoid (RecF f cs), Monoid a, Applicative f)
  => Monoid (RecF f (s :-> a ': cs)) where
  mempty = pure mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = ((<>) <$> x <*> y) :& xs <> ys
