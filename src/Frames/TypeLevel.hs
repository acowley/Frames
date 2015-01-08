{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
-- | Helpers for working with type-level lists.
module Frames.TypeLevel where

-- | Remove the first occurence of a type from a type-level list.
type family RDelete r rs where
  RDelete r (r ': rs) = rs
  RDelete r (s ': rs) = s ': RDelete r rs
