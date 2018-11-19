{-# LANGUAGE CPP, DataKinds, GeneralizedNewtypeDeriving, PatternSynonyms,
             KindSignatures, ScopedTypeVariables, TypeFamilies,
             TypeOperators #-}
-- | Column types
module Frames.Col where
import Data.Vinyl (ElField(Field), getField)
import GHC.TypeLits

-- -- | A column's type includes a textual name and the data type of each
-- -- element.
-- newtype (:->) (s::Symbol) a = Col { getCol :: a }
--   deriving (Eq,Ord,Num,Semigroup,Monoid,Real,RealFloat,RealFrac,Fractional,Floating)

-- instance forall s a. (KnownSymbol s, Show a) => Show (s :-> a) where
--   show (Col x) = symbolVal (Proxy::Proxy s)++" :-> "++show x

type (a :: Symbol) :-> b = '(a,b)

pattern Col :: KnownSymbol s => t -> ElField '(s,t)
pattern Col x = Field x

-- | Get the data payload of a named field.
getCol :: ElField '(s, t) -> t
getCol = getField

-- | Used only for a show instance that parenthesizes the value.
newtype Col' s a = Col' (ElField (s :-> a))

-- | Helper for making a 'Col''
col' :: KnownSymbol s => a -> Col' s a
col' = Col' . Field

instance (KnownSymbol s, Show a) => Show (Col' s a) where
  show (Col' c) = "(" ++ show c ++ ")"

-- | @ReplaceColumns x ys@ keeps the textual name of each element of
-- @ys@, but replaces its data type with @x@.
type family ReplaceColumns x ys where
  ReplaceColumns x '[] = '[]
  ReplaceColumns x (c :-> y ': ys) = c :-> x ': ReplaceColumns x ys
