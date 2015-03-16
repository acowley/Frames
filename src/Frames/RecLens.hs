{-# LANGUAGE ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses,
             RankNTypes,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators #-}
-- | Lens utilities for working with records.
module Frames.RecLens where
import Control.Applicative
import qualified Data.Vinyl as V
import Data.Vinyl.Functor (Identity(..))
import Data.Vinyl.TypeLevel
import Frames.Col ((:->)(..))
import Frames.Rec (Record)

rlens' :: (i ~ RIndex r rs, V.RElem r rs i, Functor f, Functor g)
       => sing r
       -> (g r -> f (g r))
       -> V.Rec g rs
       -> f (V.Rec g rs)
rlens' = V.rlens
{-# INLINE rlens' #-}

-- | Getter for a record field
rget' :: Functor g
      => (forall f. Functor f
          => (g (s :-> a) -> f (g (s :-> a))) -> V.Rec g rs -> f (V.Rec g rs))
      -> V.Rec g rs -> g a
rget' l = fmap getCol . getConst . l Const
{-# INLINE rget' #-}

-- | Setter for a record field.
rput' :: Functor g
      => (forall f. Functor f
          => (g (s :-> a) -> f (g (s :-> a))) -> V.Rec g rs -> f (V.Rec g rs))
      -> g a -> V.Rec g rs -> V.Rec g rs
rput' l y = getIdentity . l (\_ -> Identity (fmap Col y))
{-# INLINE rput' #-}

-- * Plain records

-- | Create a lens for accessing a field of a 'Rec'.
rlens :: (Functor f, V.RElem (s :-> a) rs (RIndex (s :-> a) rs))
      => proxy (s :-> a) -> (a -> f a) -> Record rs -> f (Record rs)
rlens k f = rlens' k (fmap Identity . getIdentity . fmap f')
  where f' (Col x) = fmap Col (f x)
{-# INLINE rlens #-}

-- | Getter for a record field.
rget :: (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
     -> Record rs -> a
rget l = getConst . l Const
{-# INLINE rget #-}

-- | Setter for a record field.
rput :: (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
     -> a -> Record rs -> Record rs
rput l y = getIdentity . l (\_ -> Identity y)
{-# INLINE rput #-}
