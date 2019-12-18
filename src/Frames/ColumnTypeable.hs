{-# LANGUAGE BangPatterns, DefaultSignatures, LambdaCase, TypeApplications,
             ScopedTypeVariables #-}
module Frames.ColumnTypeable where
import Control.Monad (MonadPlus)
import Data.Maybe (fromMaybe)
import Data.Readable (Readable(fromText))
import Data.Typeable (Proxy(..), typeRep, Typeable)
import qualified Data.Text as T
import Data.Int (Int32, Int64)
import Data.Vinyl.Functor (Const(..))
import Language.Haskell.TH

data Parsed a = Possibly a | Definitely a deriving (Eq, Ord, Show)

parsedValue :: Parsed a -> a
parsedValue (Possibly a) = a
parsedValue (Definitely a) = a

instance Functor Parsed where
  fmap f (Possibly x) = Possibly (f x)
  fmap f (Definitely x) = Definitely (f x)

-- | Values that can be read from a 'T.Text' with more or less
-- discrimination.
class Parseable a where
  -- | Returns 'Nothing' if a value of the given type can not be read;
  -- returns 'Just Possibly' if a value can be read, but is likely
  -- ambiguous (e.g. an empty string); returns 'Just Definitely' if a
  -- value can be read and is unlikely to be ambiguous."
  parse :: MonadPlus m => T.Text -> m (Parsed a)
  default parse :: (Readable a, MonadPlus m)
                => T.Text -> m (Parsed a)
  parse = fmap Definitely . fromText
  {-# INLINE parse #-}

  -- | Combine two parse results such that the combination can
  -- fail. Useful when we have two 'Possibly' parsed values that are
  -- different enough to suggest the parse of each should be
  -- considered a failure. The default implementation is to 'return'
  -- the first argument.
  parseCombine :: MonadPlus m => Parsed a -> Parsed a -> m (Parsed a)
  default parseCombine :: MonadPlus m => Parsed a -> Parsed a -> m (Parsed a)
  parseCombine = const . return

  representableAsType :: Parsed a -> Const TypeInfo a
  default
    representableAsType :: Typeable a
                        => Parsed a -> Const TypeInfo a
  representableAsType =
    const . Const . ExistingType . ConT . mkName . show . typeRep $ Proxy @a

-- | Discard any estimate of a parse's ambiguity.
discardConfidence :: Parsed a -> a
discardConfidence (Possibly x) = x
discardConfidence (Definitely x) = x

-- | Acts just like 'fromText': tries to parse a value from a 'T.Text'
-- and discards any estimate of the parse's ambiguity.
parse' :: (MonadPlus m, Parseable a) => T.Text -> m a
parse' = fmap discardConfidence . parse

parseIntish :: (Readable a, MonadPlus f) => T.Text -> f (Parsed a)
parseIntish t =
  Definitely <$> fromText (fromMaybe t (T.stripSuffix (T.pack ".0") t))

instance Parseable Bool where

instance Parseable Int where
  parse = parseIntish
instance Parseable Int32 where
  parse = parseIntish
instance Parseable Int64 where
  parse = parseIntish
instance Parseable Integer where
  parse = parseIntish

instance Parseable Float where
instance Parseable Double where
  -- Some CSV's export Doubles in a format like '1,000.00', filtering
  -- out commas lets us parse those sucessfully
  parse = fmap Definitely . fromText . T.filter (/= ',')
instance Parseable T.Text where

-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> TypeInfo
  inferType :: T.Text -> a

data TypeInfo
  = TypeGenerator (String -> Q [Dec])
  | ExistingType Type

instance Show TypeInfo where
  show (TypeGenerator _) = "cat"
  show (ExistingType t) = show t

existingTypeWithName :: Name -> TypeInfo
existingTypeWithName = ExistingType . ConT

existingTypeNamed :: String -> TypeInfo
existingTypeNamed = existingTypeWithName . mkName

getType :: TypeInfo -> Type
getType (TypeGenerator _) = ConT . mkName $ "Categorical"
getType (ExistingType t) = t

getExistingType :: TypeInfo -> Maybe Type
getExistingType (TypeGenerator _) = Nothing
getExistingType (ExistingType t) = Just t

getColType :: String -> TypeInfo -> Type
getColType qualName (TypeGenerator _) = ConT (mkName qualName)
getColType _ (ExistingType t) = t

getExtraDecs :: String -> TypeInfo -> Q [Dec]
getExtraDecs qualName (TypeGenerator typeGen) = typeGen qualName
getExtraDecs _ _ = pure []
