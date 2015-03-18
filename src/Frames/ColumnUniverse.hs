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
import Frames.RecF (reifyDict)
import Frames.TypeLevel (LAll)
import Data.Typeable (TypeRep)
import Data.Maybe (fromMaybe)

-- * TypeRep Helpers

-- | A 'TypeRep' tagged with the type it is associated with.
type Typed = Const TypeRep

mkTyped :: forall a. Typeable a => Typed a
mkTyped = Const (typeRep (Proxy::Proxy a))

quoteType :: TypeRep -> Q Type
quoteType x = do n <- lookupTypeName s
                 case n of
                   Just n' -> conT n'
                   Nothing -> error $ "Type "++s++" isn't in scope"
  where s = showsTypeRep x ""

-- * Readable Proxy

-- | Extract a function to test whether some value of a given type
-- could be read from some 'T.Text'.
inferReadable :: forall a. Readable a => T.Text -> Maybe (Proxy a)
inferReadable = fmap (const Proxy) . (fromText :: T.Text -> Maybe a)

-- | Helper to call 'inferReadable' on variants of a 'CoRec'.
inferReadable' :: Readable a => (((->) T.Text) :. (Maybe :. Proxy)) a
inferReadable' = Compose (Compose . inferReadable)

-- * Record Helpers

tryParseAll :: forall ts. (RecApplicative ts, LAll Readable ts)
            => T.Text -> Rec (Maybe :. Proxy) ts
tryParseAll = rtraverse getCompose funs
  where funs :: Rec (((->) T.Text) :. (Maybe :. Proxy)) ts
        funs = reifyDict (Proxy::Proxy Readable) inferReadable'

-- | Preserving the outermost functor layer, replace each element with
-- its TypeRep.
elementTypes :: (Functor f, LAll Typeable ts)
             => Rec (f :. g) ts -> Rec (f :. Typed) ts
elementTypes RNil = RNil
elementTypes (Compose x :& xs) =
    Compose (fmap (const mkTyped) x) :& elementTypes xs

-- * Column Type Inference

-- | Information necessary for synthesizing row types and comparing
-- types.
newtype ColInfo a = ColInfo (Q Type, Typed a)

-- | We use a join semi-lattice on types for representations. The
-- bottom of the lattice is effectively an error (we have nothing to
-- represent), @Bool < Int@, @Int < Double@, and @forall n. n <= Text@.
lubTypeReps :: TypeRep -> TypeRep -> Maybe Ordering
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

instance (T.Text ∈ ts) => Monoid (CoRec ColInfo ts) where
  mempty = Col (ColInfo ([t|T.Text|], mkTyped) :: ColInfo T.Text)
  mappend x@(Col (ColInfo (_, Const trX))) y@(Col (ColInfo (_, Const trY))) =
      case lubTypeReps trX trY of
        Just GT -> x
        Just LT -> y
        Just EQ -> x
        Nothing -> mempty

-- | Find the best (i.e. smallest) 'CoRec' variant to represent a
-- parsed value.
bestRep :: forall ts.
           (LAll Readable ts, LAll Typeable ts, FoldRec ts ts,
            RecApplicative ts, T.Text ∈ ts)
        => T.Text -> CoRec ColInfo ts
bestRep = aux
        . fromMaybe (Col (mkTyped :: Typed T.Text))
        . firstField
        . elementTypes
        . (tryParseAll :: T.Text -> Rec (Maybe :. Proxy) ts)
  where aux :: CoRec Typed ts -> CoRec ColInfo ts
        aux (Col d@(Const tr)) = Col (ColInfo (quoteType tr, d))
{-# INLINABLE bestRep #-}

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
