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
module Frames.ByteString.ColumnUniverse (CoRec, Columns, ColumnUniverse, CommonColumns, 
                              parsedTypeRep) where
import Language.Haskell.TH
#if __GLASGOW_HASKELL__ < 800
import Data.Monoid
#endif
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable (Typeable, showsTypeRep, typeRep)
import Data.Vinyl
import Data.Vinyl.Functor
import qualified Data.ByteString.Char8 as C8
import Frames.ByteString.CoRec
import Frames.ByteString.ColumnTypeable
import Frames.ByteString.RecF (reifyDict)
import Frames.ByteString.TypeLevel (LAll)
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
-- could be read from some 'C8.ByteString'.
inferParseable :: forall a. Parseable a
               => C8.ByteString -> (Maybe :. (Parsed :. Proxy)) a
inferParseable = Compose
               . fmap (Compose . fmap (const Proxy))
               . (parse :: C8.ByteString -> Maybe (Parsed a))

-- | Helper to call 'inferParseable' on variants of a 'CoRec'.
inferParseable' :: Parseable a
                => (((->) C8.ByteString) :. (Maybe :. (Parsed :. Proxy))) a
inferParseable' = Compose inferParseable

-- * Record Helpers

tryParseAll :: forall ts. (RecApplicative ts, LAll Parseable ts)
            => C8.ByteString -> Rec (Maybe :. (Parsed :. Proxy)) ts
tryParseAll = rtraverse getCompose funs
  where funs :: Rec (((->) C8.ByteString) :. (Maybe :. (Parsed :. Proxy))) ts
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

parsedTypeRep :: ColInfo a -> Parsed TypeRep
parsedTypeRep (ColInfo (_,p)) = fmap getConst p

-- | We use a join semi-lattice on types for representations. The
-- bottom of the lattice is effectively an error (we have nothing to
-- represent), @Bool < Int@, @Int < Double@, and @forall n. n <= Text@.
--
-- The high-level goal here is that we will pick the "greater" of two
-- choices in 'bestRep'. A 'Definitely' parse result is preferred over
-- a 'Possibly' parse result. If we have two distinct 'Possibly' parse
-- results, we give up. If we have two distinct 'Definitely' parse
-- results, we are in dangerous waters: all data is parseable at
-- /both/ types, so which do we default to? The defaulting choices
-- made here are described in the previous paragraph. If there is no
-- defaulting rule, we give up (i.e. use 'C8.ByteString' as a
-- representation).
lubTypeReps :: Parsed TypeRep -> Parsed TypeRep -> Maybe Ordering
lubTypeReps (Possibly _) (Definitely _) = Just LT
lubTypeReps (Definitely _) (Possibly _) = Just GT
lubTypeReps (Possibly trX) (Possibly trY)
  | trX == trY = Just EQ
  | otherwise = Nothing
lubTypeReps (Definitely trX) (Definitely trY)
  | trX == trY = Just EQ
  | trX == trInt  && trY == trDbl = Just LT
  | trX == trDbl  && trY == trInt = Just GT
  | trX == trBool && trY == trInt = Just LT
  | trX == trInt  && trY == trBool = Just GT
  | trX == trBool && trY == trDbl = Just LT
  | trX == trDbl  && trY == trBool = Just GT
  | otherwise = Nothing
  where trInt = typeRep (Proxy :: Proxy Int)
        trDbl = typeRep (Proxy :: Proxy Double)
        trBool = typeRep (Proxy :: Proxy Bool)

instance (C8.ByteString ∈ ts) => Monoid (CoRec ColInfo ts) where
  mempty = Col (ColInfo ([t|C8.ByteString|], Possibly mkTyped) :: ColInfo C8.ByteString)
  mappend x@(Col (ColInfo (_, trX))) y@(Col (ColInfo (_, trY))) =
      case lubTypeReps (fmap getConst trX) (fmap getConst trY) of
        Just GT -> x
        Just LT -> y
        Just EQ -> x
        Nothing -> mempty

-- | Find the best (i.e. smallest) 'CoRec' variant to represent a
-- parsed value. For inspection in GHCi after loading this module,
-- consider this example:
-- 
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
-- >>> import Frames.CoRec (foldCoRec)
-- >>> foldCoRec parsedTypeRep (bestRep @CommonColumns "2.3")
-- Definitely Double
bestRep :: forall ts.
           (LAll Parseable ts, LAll Typeable ts, FoldRec ts ts,
            RecApplicative ts, C8.ByteString ∈ ts)
        => C8.ByteString -> CoRec ColInfo ts
bestRep t
  | C8.null t = aux (Col (Compose (Possibly (mkTyped :: Typed C8.ByteString))))
  | otherwise = aux
              . fromMaybe (Col (Compose $ Possibly (mkTyped :: Typed C8.ByteString)))
              . firstField
              . elementTypes
              . (tryParseAll :: C8.ByteString -> Rec (Maybe :. (Parsed :. Proxy)) ts)
              $ t
  where aux :: CoRec (Parsed :. Typed) ts -> CoRec ColInfo ts
        aux (Col (Compose d@(Possibly (Const tr)))) =
          Col (ColInfo (quoteType tr, d))
        aux (Col (Compose d@(Definitely (Const tr)))) =
          Col (ColInfo (quoteType tr, d))
{-# INLINABLE bestRep #-}

instance (LAll Parseable ts, LAll Typeable ts, FoldRec ts ts,
          RecApplicative ts, C8.ByteString ∈ ts) =>
    ColumnTypeable (CoRec ColInfo ts) where
  colType (Col (ColInfo (t, _))) = t
  {-# INLINE colType #-}
  inferType = bestRep
  {-# INLINABLE inferType #-}

-- * Common Columns

-- | Common column types
type CommonColumns = [Bool, Int, Double, C8.ByteString]

-- | Define a set of variants that captures all possible column types.
type ColumnUniverse = CoRec ColInfo

-- | A universe of common column variants. These are the default
-- column types that @Frames@ can infer. See the
-- <http://acowley.github.io/Frames/#sec-4 Tutorial> for an example of
-- extending the default types with your own.
type Columns = ColumnUniverse CommonColumns
