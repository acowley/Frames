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
import Frames.Rec (Rec)
import Frames.RecF (RecF)

rlens' :: (i ~ RIndex r rs, r ~ (s :-> a), V.RElem r rs i, Functor f, Functor g)
       => sing (s :-> a)
       -> (g a -> f (g a))
       -> RecF g rs
       -> f (RecF g rs)
rlens' s f = V.rlens s (fmap (fmap Col) . f . fmap getCol)
{-# INLINE rlens' #-}

-- | Getter for a record field
rget' :: Functor g
      => (forall f. Functor f
          => (g (s :-> a) -> f (g (s :-> a))) -> RecF g rs -> f (RecF g rs))
      -> RecF g rs -> g a
rget' l = fmap getCol . getConst . l Const
{-# INLINE rget' #-}

-- | Setter for a record field.
rput' :: Functor g
      => (forall f. Functor f
          => (g (s :-> a) -> f (g (s :-> a))) -> RecF g rs -> f (RecF g rs))
      -> g a -> RecF g rs -> RecF g rs
rput' l y = getIdentity . l (\_ -> Identity (fmap Col y))
{-# INLINE rput' #-}

-- * Plain records

-- | Create a lens for accessing a field of a 'Rec'.
rlens :: (Functor f, V.RElem (s :-> a) rs (RIndex (s :-> a) rs))
      => proxy (s :-> a) -> (a -> f a) -> Rec rs -> f (Rec rs)
rlens k f = rlens' k (fmap Identity . getIdentity . fmap f)
{-# INLINE rlens #-}

-- | Getter for a record field.
rget :: (forall f. Functor f => (a -> f a) -> Rec rs -> f (Rec rs))
     -> Rec rs -> a
rget l = getConst . l Const
{-# INLINE rget #-}

-- | Setter for a record field.
rput :: (forall f. Functor f => (a -> f a) -> Rec rs -> f (Rec rs))
     -> a -> Rec rs -> Rec rs
rput l y = getIdentity . l (\_ -> Identity y)
{-# INLINE rput #-}
