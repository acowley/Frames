{-# LANGUAGE CPP, DataKinds, TypeFamilies, TypeOperators #-}
-- | Helpers for working with type-level lists.
module Frames.TypeLevel where
#if __GLASGOW_HASKELL__ < 800
import GHC.Prim (Constraint)
#else
import Data.Kind (Constraint)
#endif

-- | Remove the first occurence of a type from a type-level list.
type family RDelete r rs where
  RDelete r (r ': rs) = rs
  RDelete r (s ': rs) = s ': RDelete r rs

-- | A constraint on each element of a type-level list.
type family LAll c ts :: Constraint where
  LAll c '[] = ()
  LAll c (t ': ts) = (c t, LAll c ts)

-- | Constraint that every element of a promoted list is equal to a
-- particular type. That is, the list of types is a single type
-- repeated some number of times.
type family AllAre a ts :: Constraint where
  AllAre a '[] = ()
  AllAre a (t ': ts) = (t ~ a, AllAre a ts)

-- | Compound constraint that a type has an instance for each of a
-- list of type classes.
type family HasInstances a cs :: Constraint where
  HasInstances a '[] = ()
  HasInstances a (c ': cs) = (c a, HasInstances a cs)

-- | Compound constraint that all types have instances for each of a
-- list of type clasesses. @AllHave classes types@.
type family AllHave cs as :: Constraint where
  AllHave cs '[] = ()
  AllHave cs (a ': as) = (HasInstances a cs, AllHave cs as)
  
