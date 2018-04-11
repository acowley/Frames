{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds,
             FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables,
             TypeApplications, TypeFamilies, TypeOperators #-}
-- | Lens utilities for working with 'Record's.
module Frames.RecLens where
import Control.Applicative
import qualified Data.Vinyl as V
import Data.Vinyl.Functor (Identity(..))
import Data.Vinyl.TypeLevel
import Frames.Col ((:->)(..))
import Frames.Rec (Record)

rlens' :: forall r rs f g i.
          (i ~ RIndex r rs, V.RElem r rs i, Functor f)
       => (g r -> f (g r))
       -> V.Rec g rs
       -> f (V.Rec g rs)
rlens' = V.rlens
{-# INLINE rlens' #-}

-- | Getter for a 'V.Rec' field
rget' :: Functor g
      => (forall f. Functor f
          => (g (s :-> a) -> f (g (s :-> a))) -> V.Rec g rs -> f (V.Rec g rs))
      -> V.Rec g rs -> g a
rget' l = fmap getCol . getConst . l Const
{-# INLINE rget' #-}

-- | Setter for a 'V.Rec' field.
rput' :: Functor g
      => (forall f. Functor f
          => (g (s :-> a) -> f (g (s :-> a))) -> V.Rec g rs -> f (V.Rec g rs))
      -> g a -> V.Rec g rs -> V.Rec g rs
rput' l y = getIdentity . l (\_ -> Identity (fmap Col y))
{-# INLINE rput' #-}

-- * Plain records

-- | Create a lens for accessing a field of a 'Record'. The first
-- explicit type parameter is used to help with visible type
-- applications using the @TypeApplications@ language
-- extension. Typical usage might be, @rlens \@("name" :-> String)@.
rlens :: forall q s a rs f.
         (q ~ (s :-> a), Functor f, V.RElem (s :-> a) rs (RIndex (s :-> a) rs))
      => (a -> f a) -> Record rs -> f (Record rs)
rlens f = rlens' @(s :-> a) (fmap Identity . getIdentity . fmap f')
  where f' (Col x) = fmap Col (f x)
{-# INLINE rlens #-}

-- | Getter for a 'Record' field.
rget :: (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
     -> Record rs -> a
rget l = getConst . l Const
{-# INLINE rget #-}

-- | Setter for a 'Record' field.
rput :: (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
     -> a -> Record rs -> Record rs
rput l y = getIdentity . l (\_ -> Identity y)
{-# INLINE rput #-}
