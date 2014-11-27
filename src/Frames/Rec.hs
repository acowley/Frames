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
import Control.Applicative
import Data.Functor.Identity
import Data.List (intercalate)
import Frames.Col
import Frames.RecF (RecF(..), rtraverse)
import GHC.Prim (Constraint)

-- | A record with unadorned values.
type Rec = RecF Identity

-- | A @cons@ function for building 'Rec' values.
(&:) :: a -> Rec rs -> Rec (s :-> a ': rs)
x &: xs = Identity x :& xs
infixr 5 &:

-- | Inject each element of a 'Rec' into an 'Applicative'.
recPure :: Applicative f => Rec cs -> RecF f cs
recPure Nil = Nil
recPure (Identity x :& xs) = pure x :& recPure xs
{-# INLINABLE recPure #-}

-- | Undistribute 'Maybe' from a 'RecF' 'Maybe'. This is just a
-- specific usage of 'rtraverse', but it is quite common.
recMaybe :: RecF Maybe cs -> Maybe (Rec cs)
recMaybe = rtraverse (fmap Identity)
{-# INLINE recMaybe #-}

-- * Column Name Tagging 

-- | A record where each element carries its own column name type tag.
data RecC :: [*] -> * where
  NilC :: RecC '[]
  ConsC :: !(s :-> a) -> !(RecC cs) -> RecC (s :-> a ': cs)

-- | Attach column names to each element of a record.
recCols :: Rec cs -> RecC cs
recCols Nil = NilC
recCols (Identity x :& xs) = ConsC (Col x) (recCols xs)

-- | Remove the 'Col' newtype wrappers from a 'RecC'.
recBare :: RecC cs -> Rec cs
recBare NilC = Nil
recBare (ConsC (Col x) xs) = x &: recBare xs

-- | Witness that all elements of a list satisfy a 'Constraint'.
type family All (c :: * -> Constraint) rs :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

-- | Human-friendly display
showRec :: All Show cs => Rec cs -> String
showRec r = "{" ++ intercalate ", " (go $ recCols r) ++ "}"
  where go :: All Show cs => RecC cs -> [String]
        go NilC = []
        go (ConsC x xs) = show x : go xs

instance All Show cs => Show (Rec cs) where show = showRec
