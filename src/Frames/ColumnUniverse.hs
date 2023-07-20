{-# LANGUAGE BangPatterns, CPP, ConstraintKinds, DataKinds,
             FlexibleContexts, FlexibleInstances, GADTs, InstanceSigs,
             KindSignatures, LambdaCase, MultiParamTypeClasses,
             OverloadedStrings, QuasiQuotes, RankNTypes,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Frames.ColumnUniverse (
  CoRec, Columns, ColumnUniverse, ColInfo,
  CommonColumns, CommonColumnsCat, parsedTypeRep
) where
import Data.Maybe (fromMaybe)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup((<>)))
#endif
import qualified Data.Text as T
import Data.Vinyl
import Data.Vinyl.CoRec
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel (RIndex, NatToInt)
import Frames.ColumnTypeable
import Frames.Categorical
import Language.Haskell.TH

-- | Extract a function to test whether some value of a given type
-- could be read from some 'T.Text'.
inferParseable :: Parseable a => T.Text -> (Maybe :. Parsed) a
inferParseable = Compose . parse

-- | Helper to call 'inferParseable' on variants of a 'CoRec'.
inferParseable' :: Parseable a => (((->) T.Text) :. (Maybe :. Parsed)) a
inferParseable' = Compose inferParseable

-- * Record Helpers

tryParseAll :: forall ts. (RecApplicative ts, RPureConstrained Parseable ts)
            => T.Text -> Rec (Maybe :. Parsed) ts
tryParseAll = rtraverse getCompose funs
  where funs :: Rec (((->) T.Text) :. (Maybe :. Parsed)) ts
        funs = rpureConstrained @Parseable inferParseable'

-- * Column Type Inference

-- | Information necessary for synthesizing row types and comparing
-- types.
newtype ColInfo a = ColInfo (Either (String -> Q [Dec]) Type, Parsed a)
instance Show a => Show (ColInfo a) where
  show (ColInfo (t,p)) = "(ColInfo {"
                         ++ either (const "cat") show t
                         ++ ", "
                         ++ show (discardConfidence p) ++"})"

parsedToColInfo :: Parseable a => Parsed a -> ColInfo a
parsedToColInfo x = case getConst rep of
                      Left dec -> ColInfo (Left dec, x)
                      Right ty ->
                        ColInfo (Right ty, x)
  where rep = representableAsType x

parsedTypeRep :: ColInfo a -> Parsed Type
parsedTypeRep (ColInfo (t,p)) =
  const (either (const (ConT (mkName "Categorical"))) id t) <$> p

-- | Map 'Type's we know about (with a special treatment of
-- synthesized types for categorical variables) to 'Int's for ordering
-- purposes.
orderParsePriorities :: Parsed (Maybe Type) -> Maybe Int
orderParsePriorities x =
  case discardConfidence x of
    Nothing -> Just 1 -- categorical variable
    Just t
      | t == tyText -> Just (0 + uncertainty)
      | t == tyDbl -> Just (2 + uncertainty)
      | t == tyInt -> Just (3 + uncertainty)
      | t == tyBool -> Just (4 + uncertainty)
      | otherwise -> Nothing
  where tyText = ConT (mkName "Text")
        tyDbl = ConT (mkName "Double")
        tyInt = ConT (mkName "Int")
        tyBool = ConT (mkName "Bool")
        uncertainty = case x of Definitely _ -> 0; Possibly _ -> 5

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
-- defaulting rule, we give up (i.e. use 'T.Text' as a
-- representation).
lubTypes :: Parsed (Maybe Type) -> Parsed (Maybe Type) -> Maybe Ordering
lubTypes x y = compare <$> orderParsePriorities y <*> orderParsePriorities x

instance (T.Text ∈ ts, RPureConstrained Parseable ts) => Monoid (CoRec ColInfo ts) where
  mempty = CoRec (ColInfo ( Right (ConT (mkName "Text")), Possibly T.empty))

-- | A helper For the 'Semigroup' instance below.
mergeEqTypeParses :: forall ts. (RPureConstrained Parseable ts, T.Text ∈ ts)
                  => CoRec ColInfo ts -> CoRec ColInfo ts -> CoRec ColInfo ts
mergeEqTypeParses x@(CoRec _) y = fromMaybe definitelyText
                                $ coRecTraverse getCompose
                                                (coRecMapC @Parseable aux x)
  where definitelyText = CoRec (ColInfo (Right (ConT (mkName "Text")), Definitely T.empty))
        aux :: forall a. (Parseable a, NatToInt (RIndex a ts))
            => ColInfo a -> (Maybe :. ColInfo) a
        aux (ColInfo (_, pX)) =
          case asA' @a y of
            Nothing -> Compose Nothing
            Just (ColInfo (_, pY)) ->
              maybe (Compose Nothing)
                    (Compose . Just . parsedToColInfo)
                    (parseCombine pX pY)

instance (T.Text ∈ ts, RPureConstrained Parseable ts)
  => Semigroup (CoRec ColInfo ts) where
  x@(CoRec (ColInfo (tyX, pX))) <> y@(CoRec (ColInfo (tyY, pY))) =
    case lubTypes (const (either (const Nothing) Just tyX) <$> pX)
                  (const (either (const Nothing) Just tyY) <$> pY) of
      Just GT -> x
      Just LT -> y
      Just EQ -> mergeEqTypeParses x y
      Nothing -> mempty

-- | Find the best (i.e. smallest) 'CoRec' variant to represent a
-- parsed value. For inspection in GHCi after loading this module,
-- consider this example:
--
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
-- >>> import Data.Vinyl.CoRec (foldCoRec)
-- >>> foldCoRec parsedTypeRep (bestRep @CommonColumns "2.3")
-- Definitely Double
bestRep :: forall ts.
           (RPureConstrained Parseable ts,
            FoldRec ts ts,
            RecApplicative ts, T.Text ∈ ts)
        => T.Text -> CoRec ColInfo ts
bestRep t
  | T.null t || t == "NA" = (CoRec (parsedToColInfo (Possibly T.empty)))
  | otherwise = coRecMapC @Parseable parsedToColInfo
              . fromMaybe (CoRec (Possibly T.empty :: Parsed T.Text))
              . firstField
              . (tryParseAll :: T.Text -> Rec (Maybe :. Parsed) ts)
              $ t
{-# INLINABLE bestRep #-}

instance (RPureConstrained Parseable ts, FoldRec ts ts,
          RecApplicative ts, T.Text ∈ ts) =>
    ColumnTypeable (CoRec ColInfo ts) where
  colType (CoRec (ColInfo (t, _))) = t
  {-# INLINE colType #-}
  inferType = bestRep
  {-# INLINABLE inferType #-}

#if !MIN_VERSION_vinyl(0,11,0)
instance forall ts. (RPureConstrained Show ts, RecApplicative ts)
  => Show (CoRec ColInfo ts) where
  show x = "(Col " ++ onCoRec @Show show x ++")"
#endif  

-- * Common Columns

-- | Common column types: 'Bool', 'Int', 'Double', 'T.Text'
type CommonColumns = [Bool, Int, Double, T.Text]

-- | Common column types including categorical types.
type CommonColumnsCat = [Bool, Int, Double, Categorical 8, T.Text]

-- | Define a set of variants that captures all possible column types.
type ColumnUniverse = CoRec ColInfo

-- | A universe of common column variants. These are the default
-- column types that @Frames@ can infer. See the
-- <http://acowley.github.io/Frames/#sec-4 Tutorial> for an example of
-- extending the default types with your own.
type Columns = ColumnUniverse CommonColumns
