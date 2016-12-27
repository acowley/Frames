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
module Frames.ByteString.Rec where
import Data.Proxy
import Data.Vinyl (recordToList, rmap, reifyConstraint, Dict(..), Rec)
import Data.Vinyl.TypeLevel (RecAll)
import Data.Vinyl.Functor (Identity(..), Const(..), Compose(..), (:.))
import Frames.ByteString.Col
import Frames.ByteString.RecF

-- | A record with unadorned values. This is @Vinyl@'s 'Rec'
-- 'Identity'. We give this type a name as it is used pervasively for
-- records in 'Frames'.
type Record = Rec Identity

-- | A @cons@ function for building 'Record' values.
(&:) :: a -> Record rs -> Record (s :-> a ': rs)
x &: xs = frameCons (Identity x) xs
infixr 5 &:

type family RecordColumns t where
  RecordColumns (Record ts) = ts

-- | Separate the first element of a 'Record' from the rest of the row.
recUncons :: Record (s :-> a ': rs) -> (a, Record rs)
recUncons (Identity x :& xs) = (x, xs)
recUncons x = case x of

-- | Undistribute 'Maybe' from a 'Rec' 'Maybe'. This is just a
-- specific usage of 'rtraverse', but it is quite common.
recMaybe :: Rec Maybe cs -> Maybe (Record cs)
recMaybe = rtraverse (fmap Identity)
{-# INLINE recMaybe #-}

-- | Show each field of a 'Record' /without/ its column name.
showFields :: (RecAll Identity (UnColumn ts) Show, AsVinyl ts)
           => Record ts -> [String]
showFields = recordToList . rmap aux . reifyConstraint p . toVinyl
  where p = Proxy :: Proxy Show
        aux :: (Dict Show :. Identity) a -> Const String a
        aux (Compose (Dict x)) = Const (show x)
{-# INLINABLE showFields #-}
