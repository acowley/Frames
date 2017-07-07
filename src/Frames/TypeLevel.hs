{-# LANGUAGE CPP, DataKinds, TypeFamilies, TypeOperators #-}
-- | Helpers for working with type-level lists.
module Frames.TypeLevel where
#if __GLASGOW_HASKELL__ < 800
import GHC.Prim (Constraint)
#else
import Data.Kind (Constraint)
#endif

-- | Constraint that every element of a promoted list is equal to a
-- particular type. That is, the list of types is a single type
-- repeated some number of times.
type family AllAre a ts :: Constraint where
  AllAre a '[] = ()
  AllAre a (t ': ts) = (t ~ a, AllAre a ts)
