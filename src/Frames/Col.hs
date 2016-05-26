{-# LANGUAGE CPP,
             DataKinds,
             GeneralizedNewtypeDeriving,
             KindSignatures,
             ScopedTypeVariables,
             TypeOperators #-}
-- | Column types
module Frames.Col where
#if __GLASGOW_HASKELL__ < 800
import Data.Monoid
#endif
import Data.Proxy
import GHC.TypeLits

-- | A column's type includes a textual name and the data type of each
-- element.
newtype (:->) (s::Symbol) a = Col { getCol :: a }
  deriving (Eq,Ord,Num,Monoid,Real,RealFloat,RealFrac,Fractional,Floating)

instance forall s a. (KnownSymbol s, Show a) => Show (s :-> a) where
  show (Col x) = symbolVal (Proxy::Proxy s)++" :-> "++show x

-- | Used only for a show instance that parenthesizes the value.
newtype Col' s a = Col' (s :-> a)

-- | Helper for making a 'Col''
col' :: a -> Col' s a
col' = Col' . Col

instance (KnownSymbol s, Show a) => Show (Col' s a) where
  show (Col' c) = "(" ++ show c ++ ")"
