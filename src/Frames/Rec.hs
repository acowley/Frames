{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Vinyl hiding (rget)
import qualified Data.Vinyl as V
import Data.Vinyl.Functor (Const(..), Compose(..), (:.))
import Data.Vinyl.Class.Method (PayloadType)
import Frames.Col
import GHC.TypeLits (KnownSymbol)

-- | A record with unadorned values. This is @Vinyl@'s 'Rec'
-- 'ElField'. We give this type a name as it is used pervasively for
-- records in 'Frames'.
type Record = FieldRec

-- | A @cons@ function for building 'Record' values.
(&:) :: KnownSymbol s => a -> Record rs -> Record (s :-> a ': rs)
x &: xs = Field x :& xs
infixr 5 &:

type family RecordColumns t where
  RecordColumns (Record ts) = ts

-- | Separate the first element of a 'Record' from the rest of the row.
recUncons :: Record (s :-> a ': rs) -> (a, Record rs)
recUncons (Field x :& xs) = (x, xs)
-- recUncons x = case x of _ -> error "recUncons impossible case"

-- | Undistribute 'Maybe' from a 'Rec' 'Maybe'. This is just a
-- specific usage of 'rtraverse', but it is quite common.
recMaybe :: Rec (Maybe :. ElField) cs -> Maybe (Record cs)
recMaybe = rtraverse getCompose
{-# INLINE recMaybe #-}

-- | Show each field of a 'Record' /without/ its column name.
showFields :: (RecMapMethod Show ElField ts) => Record ts -> [String]
showFields = recordToList . rmapMethod @Show aux
  where aux :: (Show (PayloadType ElField a)) => ElField a -> Const String a
        aux (Field x) = Const (show x)
{-# INLINABLE showFields #-}

-- | Get the value of a field of a 'Record'.
rget :: forall t s a rs. (t ~ '(s,a), t ∈ rs) => Record rs -> a
rget = getField . V.rget @t
{-# INLINE rget #-}

-- | Replace the value of a field of a 'Record'.
rput :: forall t s a rs. (t ~ '(s,a), t ∈ rs, KnownSymbol s)
     => a -> Record rs -> Record rs
rput = V.rput @t . Field
{-# INLINE rput #-}
