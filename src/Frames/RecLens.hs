{-# LANGUAGE ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses,
             RankNTypes,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators #-}
-- | Lens utilities for working with records. This is lightly adapated
-- from Jon Sterling's work on the Vinyl library.
module Frames.RecLens where
import Control.Applicative
import Data.Functor.Identity
import Data.Proxy
import Frames.Col ((:->))
import Frames.Rec (Rec)
import Frames.RecF (RecF(..))
import Frames.TypeLevel (RImage, RIndex, Peano(..))

-- | The presence of a field in a record is witnessed by a lens into its value.
-- The third parameter to 'RElem', @i@, is there to help the constraint solver
-- realize that this is a decidable predicate with respect to the judgemental
-- equality in @*@.
class i ~ RIndex r rs => RElem r rs (i :: Peano) where

  -- | We can get a lens for getting and setting the value of a field which is
  -- in a record. As a convenience, we take a proxy argument to fix the
  -- particular field being viewed. These lenses are compatible with the @lens@
  -- library. Morally:
  --
  -- > rlens' proxy :: Lens' (RecF f rs) (f r)
  rlens' :: (r ~ (s :-> a), Functor f)
         => sing (s :-> a)
         -> (g a -> f (g a))
         -> RecF g rs
         -> f (RecF g rs)

-- | Getter for a record field
rget' :: (forall f. Functor f => (g a -> f (g a)) -> RecF g rs -> f (RecF g rs))
      -> RecF g rs -> g a
rget' l = getConst . l Const
{-# INLINE rget' #-}

-- | Setter for a record field.
rput' :: (forall f. Functor f => (g a -> f (g a)) -> RecF g rs -> f (RecF g rs))
      -> g a -> RecF g rs -> RecF g rs
rput' l y = runIdentity . l (\_ -> Identity y)
{-# INLINE rput' #-}

instance RElem r (r ': rs) Z where
  rlens' _ f (x :& xs) = fmap (:& xs) (f x)
  {-# INLINE rlens' #-}

instance (RIndex r (s ': rs) ~ S i, RElem r rs i)
  => RElem r (s ': rs) (S i) where
  rlens' p f (x :& xs) = fmap (x :&) (rlens' p f xs)
  {-# INLINE rlens' #-}

-- | If one field set is a subset another, then a lens of from the latter's
-- record to the former's is evident. That is, we can either cast a larger
-- record to a smaller one, or we may replace the values in a slice of a
-- record.
class is ~ RImage rs ss => RSubset rs ss is where

  -- | This is a lens into a slice of the larger record. Morally, we have:
  --
  -- > rsubset :: Lens' (RecF f ss) (RecF f rs)
  rsubset
    :: Functor f
    => (RecF g rs -> f (RecF g rs))
    -> RecF g ss
    -> f (RecF g ss)

  -- | The getter of the 'rsubset' lens is 'rcast', which takes a larger record
  -- to a smaller one by forgetting fields.
  rcast
    :: RecF g ss
    -> RecF g rs
  rcast = getConst . rsubset Const
  {-# INLINE rcast #-}

  -- | The setter of the 'rsubset' lens is 'rreplace', which allows a slice of
  -- a record to be replaced with different values.
  rreplace
    :: RecF g rs
    -> RecF g ss
    -> RecF g ss
  rreplace rs = runIdentity . rsubset (\_ -> Identity rs)
  {-# INLINE rreplace #-}

-- | This is an internal convenience stolen from the @lens@ library.
lens :: Functor f
     => (t -> s)
     -> (t -> a -> b)
     -> (s -> f a)
     -> t
     -> f b
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}

instance RSubset '[] ss '[] where
  rsubset = lens (const Nil) const
  {-# INLINE rsubset #-}

instance (RElem (s :-> a) ss i, RSubset rs ss is)
  => RSubset (s :-> a ': rs) ss (i ': is) where
  rsubset = lens (\ss -> rget' (rlens' (Proxy::Proxy (s :-> a))) ss :& rcast ss) set
    where set :: RecF g ss -> RecF g (s :-> a ': rs) -> RecF g ss
          set ss (r :& rs) = rput' (rlens' (Proxy::Proxy (s :-> a))) r $ rreplace rs ss
  {-# INLINE rsubset #-}

-- * Plain records

-- | Create a lens for accessing a field of a 'Rec'.
rlens :: (Functor f, RElem (s :-> a) rs (RIndex (s :-> a) rs))
      => proxy (s :-> a) -> (a -> f a) -> Rec rs -> f (Rec rs)
rlens k f = rlens' k (fmap Identity . runIdentity . fmap f)
{-# INLINE rlens #-}

-- | Getter for a record field.
rget :: (forall f. Functor f => (a -> f a) -> Rec rs -> f (Rec rs))
     -> Rec rs -> a
rget l = getConst . l Const
{-# INLINE rget #-}

-- | Setter for a record field.
rput :: (forall f. Functor f => (a -> f a) -> Rec rs -> f (Rec rs))
     -> a -> Rec rs -> Rec rs
rput l y = runIdentity . l (\_ -> Identity y)
{-# INLINE rput #-}

-- * Notational Convenience

-- | Two record types are equivalent when they are subtypes of each other.
type REquivalent rs ss is js = (RSubset rs ss is, RSubset ss rs js)

-- | A shorthand for 'RElem' which supplies its index.
type r ∈ rs = RElem r rs (RIndex r rs)

-- | A shorthand for 'RSubset' which supplies its image.
type rs ⊆ ss = RSubset rs ss (RImage rs ss)

-- | A shorthand for 'REquivalent' which supplies its images.
type rs ≅ ss = REquivalent rs ss (RImage rs ss) (RImage ss rs)

-- | A non-unicode equivalent of @(⊆)@.
type rs <: ss = rs ⊆ ss

-- | A non-unicode equivalent of @(≅)@.
type rs :~: ss = rs ≅ ss
