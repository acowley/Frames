{-# LANGUAGE BangPatterns,
             ConstraintKinds,
             CPP,
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
#if __GLASGOW_HASKELL__ < 800
import Data.Monoid
#endif
import Data.Proxy
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

-- * Parseable Proxy

-- | Extract a function to test whether some value of a given type
-- could be read from some 'T.Text'.
inferParseable :: forall a. Parseable a
               => T.Text -> (Maybe :. (Parsed :. Proxy)) a
inferParseable = Compose
               . fmap (Compose . fmap (const Proxy))
               . (parse :: T.Text -> Maybe (Parsed a))

-- | Helper to call 'inferParseable' on variants of a 'CoRec'.
inferParseable' :: Parseable a
                => (((->) T.Text) :. (Maybe :. (Parsed :. Proxy))) a
inferParseable' = Compose inferParseable

-- * Record Helpers

tryParseAll :: forall ts. (RecApplicative ts, LAll Parseable ts)
            => T.Text -> Rec (Maybe :. (Parsed :. Proxy)) ts
tryParseAll = rtraverse getCompose funs
  where funs :: Rec (((->) T.Text) :. (Maybe :. (Parsed :. Proxy))) ts
        funs = reifyDict (Proxy::Proxy Parseable) inferParseable'

-- | Preserving the outermost two functor layers, replace each element with
-- its TypeRep.
elementTypes :: (Functor f, Functor g, LAll Typeable ts)
             => Rec (f :. (g :. h)) ts -> Rec (f :. (g :. Typed)) ts
elementTypes RNil = RNil
elementTypes (Compose x :& xs) =
    Compose (fmap (Compose . fmap (const mkTyped) . getCompose) x)
    :& elementTypes xs

-- * Column Type Inference

-- | Information necessary for synthesizing row types and comparing
-- types.
newtype ColInfo a = ColInfo (Q Type, Parsed (Typed a))

-- | We use a join semi-lattice on types for representations. The
-- bottom of the lattice is effectively an error (we have nothing to
-- represent), @Bool < Int@, @Int < Double@, and @forall n. n <= Text@.
lubTypeReps :: Parsed TypeRep -> Parsed TypeRep -> Maybe Ordering
lubTypeReps (Possibly _) (Definitely _) = Just LT
lubTypeReps (Definitely _) (Possibly _) = Just GT
lubTypeReps (Possibly trX) (Possibly trY)
  | trX == trY = Just EQ
  | otherwise = Nothing
lubTypeReps (Definitely trX) (Definitely trY)
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
  mempty = Col (ColInfo ([t|T.Text|], Possibly mkTyped) :: ColInfo T.Text)
  mappend x@(Col (ColInfo (_, trX))) y@(Col (ColInfo (_, trY))) =
      case lubTypeReps (fmap getConst trX) (fmap getConst trY) of
        Just GT -> x
        Just LT -> y
        Just EQ -> x
        Nothing -> mempty

-- | Find the best (i.e. smallest) 'CoRec' variant to represent a
-- parsed value.
bestRep :: forall ts.
           (LAll Parseable ts, LAll Typeable ts, FoldRec ts ts,
            RecApplicative ts, T.Text ∈ ts)
        => T.Text -> CoRec ColInfo ts
bestRep = aux
        . fromMaybe (Col (Compose $ Possibly (mkTyped :: Typed T.Text)))
        . firstField
        . elementTypes
        . (tryParseAll :: T.Text -> Rec (Maybe :. (Parsed :. Proxy)) ts)
  where aux :: CoRec (Parsed :. Typed) ts -> CoRec ColInfo ts
        aux (Col (Compose d@(Possibly (Const tr)))) =
          Col (ColInfo (quoteType tr, d))
        aux (Col (Compose d@(Definitely (Const tr)))) =
          Col (ColInfo (quoteType tr, d))
{-# INLINABLE bestRep #-}

instance (LAll Parseable ts, LAll Typeable ts, FoldRec ts ts,
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

-- | A universe of common column variants. These are the default
-- column types that @Frames@ can infer. See the
-- <http://acowley.github.io/Frames/#sec-4 Tutorial> for an example of
-- extending the default types with your own.
type Columns = ColumnUniverse CommonColumns
