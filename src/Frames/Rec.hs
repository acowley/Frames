{-# LANGUAGE ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             FunctionalDependencies,
             KindSignatures,
             GADTs,
             MultiParamTypeClasses,
             PatternSynonyms,
             PolyKinds,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Frames.Rec where
import Data.Vinyl.Functor (Identity(..))
import Frames.Col
import Frames.RecF

-- | A record with unadorned values.
type Rec = RecF Identity

-- | A @cons@ function for building 'Rec' values.
(&:) :: a -> Rec rs -> Rec (s :-> a ': rs)
x &: xs = frameCons (Identity x) xs
infixr 5 &:

-- | Undistribute 'Maybe' from a 'RecF' 'Maybe'. This is just a
-- specific usage of 'rtraverse', but it is quite common.
recMaybe :: RecF Maybe cs -> Maybe (Rec cs)
recMaybe = rtraverse (fmap Identity)
{-# INLINE recMaybe #-}
