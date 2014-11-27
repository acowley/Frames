{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
-- | Helpers for working with type-level lists.
module Frames.TypeLevel where

-- | Append type-level lists
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | The natural numbers.
data Peano = Z | S Peano

-- | A partial relation that gives the index of a value in a list.
type family RIndex r rs :: Peano where
  RIndex r (r ': rs) = Z
  RIndex r (s ': rs) = S (RIndex r rs)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family RImage rs ss :: [Peano] where
  RImage '[] ss = '[]
  RImage (r ': rs) ss = RIndex r ss ': RImage rs ss

-- | Remove the first occurence of a type from a type-level list.
type family RDelete r rs where
  RDelete r (r ': rs) = rs
  RDelete r (s ': rs) = s ': RDelete r rs
