{-# LANGUAGE ConstraintKinds,
             DataKinds,
             EmptyCase,
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
import Data.Proxy
import Data.Vinyl (recordToList, rmap, reifyConstraint, Dict(..))
import Data.Vinyl.TypeLevel (RecAll)
import Data.Vinyl.Functor (Identity(..), Const(..), Compose(..), (:.))
import Frames.Col
import Frames.RecF

-- | A record with unadorned values.
type Rec = RecF Identity

-- | A @cons@ function for building 'Rec' values.
(&:) :: a -> Rec rs -> Rec (s :-> a ': rs)
x &: xs = frameCons (Identity x) xs
infixr 5 &:

-- | Separate the first element of a 'Rec' from the rest of the row.
recUncons :: Rec (s :-> a ': rs) -> (a, Rec rs)
recUncons (Identity x :& xs) = (x, xs)
recUncons x = case x of

-- | Undistribute 'Maybe' from a 'RecF' 'Maybe'. This is just a
-- specific usage of 'rtraverse', but it is quite common.
recMaybe :: RecF Maybe cs -> Maybe (Rec cs)
recMaybe = rtraverse (fmap Identity)
{-# INLINE recMaybe #-}

-- | Show each field of a 'Rec' /without/ its column name.
showFields :: (RecAll Identity (UnColumn ts) Show, AsVinyl ts)
           => Rec ts -> [String]
showFields = recordToList . rmap aux . reifyConstraint p . toVinyl
  where p = Proxy :: Proxy Show
        aux :: (Dict Show :. Identity) a -> Const String a
        aux (Compose (Dict x)) = Const (show x)
{-# INLINABLE showFields #-}
