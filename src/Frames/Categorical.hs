{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Frames.Categorical where
import Control.Monad (MonadPlus(mzero))
import Data.Char (toUpper)
import Data.Readable (Readable)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Vinyl.Functor (Const(..))
import Frames.ColumnTypeable
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeNats
import Language.Haskell.TH

-- | A categorical variable can take on one of a finite number of
-- textual names. Any value of type @Categorical n@ has no more than
-- @n@ variants.
newtype Categorical (n :: Nat) = Categorical { categories :: Set Text }
  deriving (Eq, Show, Typeable)

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
    Const (Left (\(cap -> n) -> [dataDecl n, isString n, readable n, parseable n]))
    where variantNames = map T.unpack cats
          dataDecl typeName =
            DataD []
                  (mkName typeName)
                  [] Nothing
                  (map (toCon typeName) variantNames)
                  [DerivClause Nothing [ (ConT ''Eq)
                                       , (ConT ''Enum)
                                       , (ConT ''Bounded)
                                       , (ConT ''Ord)
                                       , (ConT ''Show) ]]
          toCon typeName variantName =
            NormalC (mkName (typeName ++ cap variantName)) []
          cap [] = []
          cap (c : cs) = toUpper c : cs
          fromStringClause typeName variantName =
            Clause [LitP (StringL variantName)]
                   (NormalB (ConE (mkName (typeName ++ cap variantName))))
                   []
          isString typeName =
            InstanceD Nothing []
                      (AppT (ConT ''IsString) (ConT (mkName typeName)))
                      [FunD (mkName "fromString")
                            (map (fromStringClause typeName) variantNames)]
          readable typeName =
            InstanceD Nothing []
                      (AppT (ConT ''Readable) (ConT (mkName typeName)))
                      [FunD (mkName "fromText")
                            [Clause []
                               (NormalB (InfixE (Just (VarE (mkName "return")))
                                                (VarE (mkName "."))
                                                (Just (InfixE (Just (VarE (mkName "fromString")))
                                                              (VarE (mkName "."))
                                                              (Just (VarE 'T.unpack))))))
                               []]]
          parseable typeName =
            InstanceD Nothing []
                      (AppT (ConT ''Parseable) (ConT (mkName typeName))) []
