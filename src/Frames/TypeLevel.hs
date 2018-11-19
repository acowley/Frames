{-# LANGUAGE CPP, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
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

-- | @ReplaceAll x ys@ produces a type-level list of the same length
-- as @ys@ where each element is @x@. In other words, it replaces each
-- element of @ys@ with @x@. This would be @map (const x) ys@ in
-- value-level Haskell.
type family ReplaceAll a xs where
  ReplaceAll a '[] = '[]
  ReplaceAll a (x ': xs) = a ': ReplaceAll a xs

-- | Replace the second component of every tuple in a type-level list
-- with a constant.
type family ReplaceAllSnd a (xs :: [(k1,k2)]) where
  ReplaceAllSnd a '[] = '[]
  ReplaceAllSnd a ('(s,x) ': xs) = '(s,a) ': ReplaceAllSnd a xs
