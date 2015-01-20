{-# LANGUAGE BangPatterns,
             ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             InstanceSigs,
             KindSignatures,
             LambdaCase,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             RankNTypes,
             ScopedTypeVariables,
             TemplateHaskell,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Frames.ColumnUniverse (CoRec, Columns, ColumnUniverse, CommonColumns) where
import Language.Haskell.TH
import Data.Monoid
import Data.Proxy
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import Data.Typeable (Typeable, showsTypeRep, typeRep)
import Data.Vinyl
import Data.Vinyl.Functor
import Frames.CoRec
import Frames.ColumnTypeable
import GHC.Prim (Constraint)
import Data.Typeable (TypeRep)

-- * Column type inference

inferReadable :: forall a. Readable a => T.Text -> Maybe (Proxy a)
inferReadable = fmap (const Proxy) . (fromText :: T.Text -> Maybe a)

-- | A constraint on each element of a type-level list.
type family LAll c ts :: Constraint where
  LAll c '[] = ()
  LAll c (t ': ts) = (c t, LAll c ts)

-- | A value carrying a typeclass dictionary
data Dict' c a where
  Dict' :: c a => Dict' c a

firstRep :: FoldRec ts ts
         => CoRec (Dict' Typeable) ts
         -> Rec (DictF Typeable (Maybe :. f)) ts
         -> CoRec (Dict' Typeable) ts
firstRep z = maybe z aux . firstField . rmap nt
  where nt :: DictF Typeable (Maybe :. f) a -> (Maybe :. (DictF Typeable f)) a
        nt (DictF (Compose x)) = Compose (fmap DictF x)
        aux :: CoRec (DictF Typeable f) ts -> CoRec (Dict' Typeable) ts
        aux (Col (DictF x)) = Col (withType x Dict')

type PartialOrd = Maybe Ordering

-- | We use a join semi-lattice on types for representations. The
-- bottom of the lattice is effectively an error (we have nothing to
-- represent), @Bool < Int@, @Int < Double@, and @forall n. n <= Text@.
lubTypeReps :: TypeRep -> TypeRep -> PartialOrd
lubTypeReps trX trY
  | trX == trY = Just EQ
  | trX == trInt && trY == trDbl = Just LT
  | trX == trDbl && trY == trInt = Just GT
  | trX == trBool && trY == trInt = Just LT
  | trX == trInt && trY == trBool = Just GT
  | otherwise = Nothing
  where trInt = typeRep (Proxy :: Proxy Int)
        trDbl = typeRep (Proxy :: Proxy Double)
        trBool = typeRep (Proxy :: Proxy Bool)

-- | Information necessary for synthesizing row types and comparing
-- types.
newtype ColInfo a = ColInfo (Q Type, Dict' Typeable a)

instance (T.Text ∈ ts) => Monoid (CoRec ColInfo ts) where
  mempty = Col (ColInfo ([t|T.Text|], Dict') :: ColInfo T.Text)
  mappend x@(Col (ColInfo (_, trX@Dict'))) y@(Col (ColInfo (_, trY@Dict'))) =
      case lubTypeReps (typeRep trX) (typeRep trY) of
        Just GT -> x
        Just LT -> y
        Just EQ -> x
        Nothing -> mempty

-- | Reify constraints on the element types themselves, rather than
-- the functor-wrapped values.
reifyConstraint' :: (LAll c ts, Functor f)
                 => proxy c -> Rec f ts -> Rec (Dict' c) ts
reifyConstraint' _ RNil = RNil
reifyConstraint' p (_ :& xs) = Dict' :& reifyConstraint' p xs

-- | Helper to call 'inferReadable' on variants of a CoRec.
inferReadable' :: (Dict' Readable) a -> (((->) T.Text) :. (Maybe :. Proxy)) a
inferReadable' Dict' = Compose (Compose . inferReadable)

tryParseAll :: forall ts. (RecApplicative ts, LAll Readable ts)
            => T.Text -> Rec (Maybe :. Proxy) ts
tryParseAll = rtraverse getCompose funs
  where funs :: Rec (((->) T.Text) :. (Maybe :. Proxy)) ts
        funs = rpure (Lift inferReadable')
               <<*>> reifyConstraint' (Proxy::Proxy Readable) (rpure Nothing)

data DictF c f a where
  DictF :: c a => f a -> DictF c f a

reifyConstraintL :: LAll c ts => proxy c -> Rec f ts -> Rec (DictF c f) ts
reifyConstraintL _ RNil = RNil
reifyConstraintL p (x :& xs) = DictF x :& reifyConstraintL p xs

withType :: proxy a -> f a -> f a
withType _ c = c

bestRep :: forall ts.
           (LAll Readable ts, LAll Typeable ts, FoldRec ts ts,
            RecApplicative ts, T.Text ∈ ts)
        => T.Text -> CoRec ColInfo ts
bestRep = aux
        . firstRep (Col (Dict' :: Dict' Typeable T.Text))
        . reifyConstraintL (Proxy :: Proxy Typeable)
        . (tryParseAll :: T.Text -> Rec (Maybe :. Proxy) ts)
  where aux :: CoRec (Dict' Typeable) ts -> CoRec ColInfo ts
        aux (Col d@Dict') = Col (withType d (ColInfo (quoteType d, d)))
{-# INLINABLE bestRep #-}

quoteType :: Typeable a => proxy a -> Q Type
quoteType x = do n <- lookupTypeName s
                 case n of
                   Just n' -> conT n'
                   Nothing -> error $ "Type "++s++" isn't in scope"
  where s = showsTypeRep (typeRep x) ""

instance (LAll Readable ts, LAll Typeable ts, FoldRec ts ts,
          RecApplicative ts, T.Text ∈ ts) =>
    ColumnTypeable (CoRec ColInfo ts) where
  colType (Col (ColInfo (t, _))) = t
  {-# INLINE colType #-}
  inferType = bestRep
  {-# INLINABLE inferType #-}

-- * Common Columns

-- | Common column types
type CommonColumns = [Bool, Int, Double, T.Text]

-- | Define a set of variants that captures all possible column types.
type ColumnUniverse = CoRec ColInfo

-- | A universe of common column variants.
type Columns = ColumnUniverse CommonColumns
