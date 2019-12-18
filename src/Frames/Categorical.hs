{-# LANGUAGE DataKinds, KindSignatures, MagicHash,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             ViewPatterns #-}
-- | Support for representing so-called categorical variables: a
-- (usually small) finite set of textual values. We map these onto
-- regular Haskell data types and offer help to generate useful type
-- class instances for such types.
module Frames.Categorical where
import Control.Applicative (ZipList(..))
import Control.DeepSeq (NFData(..))
import Control.Monad (MonadPlus(mzero))
import Data.Char (toUpper)
import Data.Readable (Readable(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Vector.Unboxed.Deriving
import Data.Vinyl.Functor (Const(..))
import Data.Word
import qualified Data.Vector.Unboxed as VU
import Frames.ColumnTypeable
import Frames.InCore (VectorFor)
import Frames.ShowCSV
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeNats
import Language.Haskell.TH

-- | A categorical variable can take on one of a finite number of
-- textual names. Any value of type @Categorical n@ has no more than
-- @n@ variants.
newtype Categorical (n :: Nat) = Categorical { categories :: Set Text }
  deriving (Eq, Show, Typeable)

-- | Ensure the first character of a 'String' is uppercase.
cap :: String -> String
cap [] = []
cap (c : cs) = toUpper c : cs

-- | Helper for working with 'derivingUnbox'. Takes the name of the
-- type and the number of variants in the sum type in order to
-- determine a compact representation.
unboxDecls :: String -> Int -> DecsQ
unboxDecls name numVariants =
  derivingUnbox name
                [t|() => $(conT (mkName name)) -> $(conT repTy)|]
                [|fromIntegral . fromEnum|]
                [|toEnum . fromIntegral|]
  where repTy
          | numVariants < 2^(8 :: Int) = ''Word8
          | numVariants < 2^(16 :: Int) = ''Word16
          | numVariants < 2^(32 :: Int) = ''Word32
          | otherwise = ''Word64

-- | Generate a splice with data type declaration and associated
-- instances for type suitable for representing a categorical
-- variable. This is a type that maps between a finite set of textual
-- names and Haskell data constructors. Usage: @declareCategorical
-- typeName optionalConPrefix variantNames@ will produce a data type
-- with name @typeName@ and data constructors whose names are a
-- concatenation of @optionalConPrefix@ and each element of
-- @variantNames@.
declareCategorical :: String -> Maybe String -> [String] -> Q [Dec]
declareCategorical (cap -> name) (fmap cap -> prefix) variants =
  ([ dataDecl, iIsString, iReadable, iParseable
   , iShowCSV, iVectorFor, iNFData ] ++)
  <$> unboxDecls name (length variants)
  where variantCons = map (mkName . maybe id (++) prefix . cap) variants
        onVariants :: (String -> Name -> a) -> [a]
        onVariants f =
          getZipList (f <$> ZipList variants <*> ZipList variantCons)
        nameName = mkName name
        fromStringClause variant variantCon =
          Clause [LitP (StringL variant)] (NormalB (ConE variantCon)) []
        showCSVClause variant variantCon =
          Clause [ConP variantCon []]
                 (NormalB (AppE (VarE 'T.pack) (LitE (StringL variant))))
                 []
        readableGuarded :: Name -> String -> Name -> (Guard, Exp)
        readableGuarded argName variant variantCon =
          ( NormalG (InfixE (Just (VarE argName))
                    (VarE '(==))
                    (Just (AppE (VarE 'T.pack) (LitE (StringL variant)))))
          , AppE (VarE 'return ) (ConE variantCon) )
        dataDecl = DataD [] nameName [] Nothing
                         (map (flip NormalC []) variantCons)
                         [DerivClause Nothing [ ConT ''Eq
                                              , ConT ''Enum
                                              , ConT ''Bounded
                                              , ConT ''Ord
                                              , ConT ''Show ]]
        iIsString =
          InstanceD Nothing [] (AppT (ConT ''IsString) (ConT nameName))
                    [FunD 'fromString
                          (onVariants fromStringClause)]
        iReadable =
          let argName = mkName "t"
              clauses = onVariants (readableGuarded argName)
              clausesTotal = clauses ++ [(NormalG (ConE 'True), VarE 'mzero)]
          in InstanceD Nothing [] (AppT (ConT ''Readable) (ConT nameName))
                       [FunD 'fromText
                             [Clause [VarP argName] (GuardedB clausesTotal) []]]
        iParseable =
          InstanceD Nothing [] (AppT (ConT ''Parseable) (ConT nameName)) []
        iShowCSV =
          InstanceD Nothing [] (AppT (ConT ''ShowCSV) (ConT nameName))
                    [FunD 'showCSV (onVariants showCSVClause)]
        iVectorFor =
          TySynInstD ''VectorFor (TySynEqn [ConT nameName] (ConT ''VU.Vector))
        iNFData =
          let argName = mkName "x"
          in InstanceD Nothing [] (AppT (ConT ''NFData) (ConT nameName))
                       [FunD 'rnf [Clause [VarP argName]
                                  (NormalB
                                   (AppE (AppE (VarE 'seq) (VarE argName))
                                         (TupE [])))
                                  []]]

instance KnownNat n => Parseable (Categorical n) where
  parse txt = return (Possibly (Categorical (S.singleton txt)))
  parseCombine p1 p2
    | S.size catCombined <= maxVariants =
      return (Possibly (Categorical catCombined))
    | otherwise = mzero
    where getCats = categories . parsedValue
          catCombined = S.union (getCats p1) (getCats p2)
          maxVariants :: Int
          maxVariants = fromIntegral (toInteger (natVal' (proxy# :: Proxy# n)))
  representableAsType (S.toList . categories . parsedValue -> cats) =
    Const . TypeGenerator $
      \n -> declareCategorical n (Just n) (map T.unpack cats)
