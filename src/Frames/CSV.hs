{-# LANGUAGE BangPatterns,
             DataKinds,
             FlexibleInstances,
             KindSignatures,
             LambdaCase,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             ScopedTypeVariables,
             TemplateHaskell,
             TypeOperators #-}
-- | Infer row types from comma-separated values (CSV) data and read
-- that data from files. Template Haskell is used to generate the
-- necessary types so that you can write type safe programs referring
-- to those types.
module Frames.CSV where
import Control.Applicative ((<$>), pure, (<*>))
import Control.Arrow (first)
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper)
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), Monoid(..), First(..))
import Data.Proxy
import Data.Readable (Readable(fromText))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (sequenceA)
import Data.Vinyl (RElem)
import Data.Vinyl.TypeLevel (RIndex)
import Frames.Col
import Frames.Rec
import Frames.RecF
import Frames.RecLens
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Pipes as P
import System.IO (Handle, hIsEOF, openFile, IOMode(..), withFile)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Control.Monad (void)

type Separator = T.Text

data ParserOptions = ParserOptions { headerOverride :: Maybe [T.Text]
                                   , columnSeparator :: Separator }
  deriving (Eq, Ord, Show)

instance Lift ParserOptions where
  lift (ParserOptions Nothing sep) = [|ParserOptions Nothing $sep'|]
    where sep' = [|T.pack $(stringE $ T.unpack sep)|]
  lift (ParserOptions (Just hs) sep) = [|ParserOptions (Just $hs') $sep'|]
    where sep' = [|T.pack $(stringE $ T.unpack sep)|]
          hs' = [|map T.pack $(listE $  map (stringE . T.unpack) hs)|]

-- | Default 'ParseOptions' get column names from a header line, and
-- use commas to separate columns.
defaultParser :: ParserOptions
defaultParser = ParserOptions Nothing (T.pack defaultSep)

-- | Default separator string.
defaultSep :: String
defaultSep = ","

-- | Helper to split a 'T.Text' on commas and strip leading and
-- trailing whitespace from each resulting chunk.
tokenizeRow :: Separator -> T.Text -> [T.Text]
tokenizeRow sep = map T.strip . T.splitOn sep

-- * Column Types

-- | This class relates a universe of possible column types to Haskell
-- types, and provides a mechanism to infer which type best represents
-- some textual data.
class ColumnTypeable a where
  colType :: a -> Q Type
  inferType :: T.Text -> a

-- | The set of supported column data types.
data ColType = TBool | TInt | TDouble | TText
  deriving (Bounded,Enum,Eq,Ord,Show)

instance Monoid ColType where
  mempty = maxBound
  mappend TInt TDouble = TDouble
  mappend TDouble TInt = TDouble
  mappend x y
      | x == y = x
      | otherwise = TText

-- | Syntax for the Haskell type corresponding to a given 'ColType'.
instance ColumnTypeable ColType where
  colType TBool = [t|Bool|]
  colType TInt = [t|Int|]
  colType TDouble = [t|Double|]
  colType TText = [t|T.Text|]
  inferType = inferColType

-- | Mapping from Haskell types to 'ColType's.
class HaskToColType a b where
  haskToColType :: proxy a -> b
instance HaskToColType Bool ColType where haskToColType _ = TBool
instance HaskToColType Int ColType where haskToColType _ = TInt
instance HaskToColType Double ColType where haskToColType _ = TDouble
instance HaskToColType T.Text ColType where haskToColType _ = TText

-- | See if some text can be parsed as a 'Bool'.
inferBool :: T.Text -> Maybe ColType
inferBool = bool Nothing (Just TBool) . (`elem` ["false", "true"]) . T.toLower

-- | See if some text can be parsed as a 'Int'.
inferInt :: T.Text -> Maybe ColType
inferInt = fmap aux . fromText
  where aux :: Int -> ColType
        aux _ = TInt

-- | See if some text can be parsed as a 'Double'.
inferDouble :: T.Text -> Maybe ColType
inferDouble = fmap aux . fromText
  where aux :: Double -> ColType
        aux _ = TDouble

-- | Determine the smallest type to represent some parsed text.
inferColType :: T.Text -> ColType
inferColType t = fromMaybe TText . getFirst $
                 foldMap (First . ($ t)) [inferBool, inferInt, inferDouble]

-- | Determine the smallest type to represent each column. NOTE: If
-- different rows are of different lengths, things will go wrong.
mergeTypes :: [[ColType]] -> [ColType]
mergeTypes [] = []
mergeTypes (x:xs) = go x xs
  where go acc [] = acc
        go acc (y:ys) = go (zipWith (<>) acc y) ys

-- | Infer column types from a prefix (up to 1000 lines) of a CSV
-- file.
prefixInference :: (ColumnTypeable a, Monoid a)
                => T.Text -> Handle -> IO [a]
prefixInference sep h = T.hGetLine h >>= go prefixSize . inferCols
  where prefixSize = 1000 :: Int
        inferCols = map inferType . tokenizeRow sep
        go 0 ts = return ts
        go !n ts =
          hIsEOF h >>= \case
            True -> return ts
            False -> T.hGetLine h >>= go (n - 1) . zipWith (<>) ts . inferCols

-- | Extract column names and inferred types from a CSV file.
readColHeaders :: (ColumnTypeable a, Monoid a)
               => ParserOptions -> FilePath -> IO [(T.Text, a)]
readColHeaders opts f =  withFile f ReadMode $ \h ->
                         zip <$> maybe (tokenizeRow sep <$> T.hGetLine h)
                                       pure
                                       (headerOverride opts)
                             <*> prefixInference sep h
  where sep = columnSeparator opts

-- * Loading Data

-- | Parsing each component of a 'RecF' from a list of text chunks,
-- one chunk per record component.
class ReadRec (rs :: [*]) where
  readRec :: [T.Text] -> RecF Maybe rs

instance ReadRec '[] where
  readRec _ = Nil

instance (Readable t, ReadRec ts) => ReadRec (s :-> t ': ts) where
  readRec [] = frameCons Nothing (readRec [])
  readRec (h:t) = frameCons (fromText h) (readRec t)

-- | Read a 'RecF' from one line of CSV.
readRow :: ReadRec rs => Separator -> T.Text -> RecF Maybe rs
readRow = (readRec .) . tokenizeRow

-- | Produce rows where any given entry can fail to parse.
readTableMaybeOpt :: (MonadIO m, ReadRec rs)
                  => ParserOptions -> FilePath -> P.Producer (RecF Maybe rs) m ()
readTableMaybeOpt opts csvFile =
  do h <- liftIO $ do
            h <- openFile csvFile ReadMode
            when (isNothing $ headerOverride opts) (void $ T.hGetLine h)
            return h
     let sep = columnSeparator opts
         go = liftIO (hIsEOF h) >>= \case
              True -> return ()
              False -> liftIO (readRow sep <$> T.hGetLine h) >>= P.yield >> go
     go
{-# INLINE readTableMaybeOpt #-}

-- | Produce rows where any given entry can fail to parse.
readTableMaybe :: (MonadIO m, ReadRec rs)
               => FilePath -> P.Producer (RecF Maybe rs) m ()
readTableMaybe = readTableMaybeOpt defaultParser
{-# INLINE readTableMaybe #-}

-- | Returns a `MonadPlus` producer of rows for which each column was
-- successfully parsed. This is typically slower than 'readTableOpt'.
readTableOpt' :: forall m rs.
                 (MonadPlus m, MonadIO m, ReadRec rs)
              => ParserOptions -> FilePath -> m (Rec rs)
readTableOpt' opts csvFile =
  do h <- liftIO $ do
            h <- openFile csvFile ReadMode
            when (isNothing $ headerOverride opts) (void $ T.hGetLine h)
            return h
     let sep = columnSeparator opts
         go = liftIO (hIsEOF h) >>= \case
              True -> mzero
              False -> let r = recMaybe . readRow sep <$> T.hGetLine h
                       in liftIO r >>= maybe go (flip mplus go . return)
     go
{-# INLINE readTableOpt' #-}

-- | Returns a `MonadPlus` producer of rows for which each column was
-- successfully parsed. This is typically slower than 'readTable'.
readTable' :: forall m rs. (MonadPlus m, MonadIO m, ReadRec rs)
           => FilePath -> m (Rec rs)
readTable' = readTableOpt' defaultParser
{-# INLINE readTable' #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTableOpt :: forall m rs.
                (MonadIO m, ReadRec rs)
             => ParserOptions -> FilePath -> P.Producer (Rec rs) m ()
readTableOpt opts csvFile = readTableMaybeOpt opts csvFile P.>-> go
  where go = P.await >>= maybe go (\x -> P.yield x >> go) . recMaybe
{-# INLINE readTableOpt #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTable :: forall m rs. (MonadIO m, ReadRec rs)
          => FilePath -> P.Producer (Rec rs) m ()
readTable = readTableOpt defaultParser
{-# INLINE readTable #-}


-- * Template Haskell

-- | Generate a column type.
recDec :: ColumnTypeable a => [(T.Text, a)] -> Q Type
recDec = appT [t|Rec|] . go
  where go [] = return PromotedNilT
        go ((n,t):cs) =
          [t|($(litT $ strTyLit (T.unpack n)) :-> $(colType t)) ': $(go cs) |]

-- | Massage a column name from a CSV file into a valid Haskell type
-- identifier.
sanitizeTypeName :: T.Text -> T.Text
sanitizeTypeName = fixupStart . T.concat . T.split (not . valid) . toTitle'
  where valid c = isAlphaNum c || c == '\'' || c == '_'
        toTitle' = foldMap (onHead toUpper) . T.split (not . isAlphaNum)
        onHead f = maybe mempty (uncurry T.cons) . fmap (first f) . T.uncons 
        fixupStart t = case T.uncons t of
                         Nothing -> "Col"
                         Just (c,_) | isAlpha c -> t
                                    | otherwise -> "Col" <> t

-- | Declare a type synonym for a column.
mkColTDec :: TypeQ -> Name -> DecQ
mkColTDec colTypeQ colTName = tySynD colTName [] colTypeQ

-- | Declare a singleton value of the given column type.
mkColPDec :: Name -> TypeQ -> T.Text -> DecsQ
mkColPDec colTName colTy colPName = sequenceA [tySig, val, tySig', val']
  where nm = mkName $ T.unpack colPName
        nm' = mkName $ T.unpack colPName <> "'"
        -- tySig = sigD nm [t|Proxy $(conT colTName)|]
        tySig = sigD nm [t|(Functor f,
                            RElem $(conT colTName) rs (RIndex $(conT colTName) rs))
                         => ($colTy -> f $colTy)
                         -> Rec rs
                         -> f (Rec rs)
                         |]
        tySig' = sigD nm' [t|(Functor f, Functor g,
                             RElem $(conT colTName) rs (RIndex $(conT colTName) rs))
                          => (g $colTy -> f (g $colTy))
                          -> RecF g rs
                          -> f (RecF g rs)
                          |]
        val = valD (varP nm)
                   (normalB [e|rlens (Proxy :: Proxy $(conT colTName))|])
                   []
        val' = valD (varP nm')
                    (normalB [e|rlens' (Proxy :: Proxy $(conT colTName))|])
                    []

-- | For each column, we declare a type synonym for its type, and a
-- Proxy value of that type.
colDec :: ColumnTypeable a => T.Text -> T.Text -> a -> DecsQ
colDec prefix colName colTy = (:) <$> mkColTDec colTypeQ colTName'
                                  <*> mkColPDec colTName' colTyQ colPName
  where colTName = sanitizeTypeName (prefix <> colName)
        colPName = fromMaybe "colDec impossible" $ 
                   fmap (\(c,t) -> T.cons (toLower c) t) (T.uncons colTName)
        colTName' = mkName $ T.unpack colTName
        colTyQ = colType colTy
        colTypeQ = [t|$(litT . strTyLit $ T.unpack colName) :-> $colTyQ|]

-- * Default CSV Parsing

-- | Generate a type for each row of a table. This will be something
-- like @Rec ["x" :-> a, "y" :-> b, "z" :-> c]@. Possible column types
-- are identified by the given proxy for a type universe. Column type
-- synonyms are /not/ generated (see 'tableTypes').
tableType' :: forall proxy a. (ColumnTypeable a, Monoid a)
           => proxy a -> String -> FilePath -> DecsQ
tableType' p = tableTypeOpt' p [] defaultSep

-- | Generate a type for each row of a table. This will be something
-- like @Rec ["x" :-> a, "y" :-> b, "z" :-> c]@.
tableType :: String -> FilePath -> DecsQ
tableType = tableTypeOpt [] defaultSep

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens (Proxy ::
-- Proxy Foo)@, and @foo' = rlens' (Proxy :: Proxy Foo)@. Possible
-- column types are identified by the given proxy for a type universe.
tableTypes' :: (ColumnTypeable a, Monoid a)
            => proxy a -> String -> FilePath -> DecsQ
tableTypes' p =  tableTypesOpt' p [] defaultSep

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens (Proxy :: Proxy
-- Foo)@, and @foo' = rlens' (Proxy :: Proxy Foo)@.
tableTypes :: String -> FilePath -> DecsQ
tableTypes = tableTypesOpt [] defaultSep

-- | Like 'tableTypes', but prefixes each column type and proxy value
-- name with the second argument. This is useful if you have very
-- generic column names. For example, @tableTypesPrefixed "Row" "Col"
-- myFile.csv@ will generate column types like @ColName@ with a
-- corresponding lens value @colName@ where @Name@ is the name of a
-- column in the CSV file.
tableTypesPrefixed' :: forall proxy a. (ColumnTypeable a, Monoid a)
                    => proxy a      -- ^ Universe of column types
                    -> String       -- ^ Row type name
                    -> String       -- ^ Column name prefix
                    -> FilePath     -- ^ CSV file
                    -> DecsQ
tableTypesPrefixed' p = tableTypesPrefixedOpt' p [] defaultSep

-- | Like 'tableTypes', but prefixes each column type and proxy value
-- name with the second argument. This is useful if you have very
-- generic column names. For example, @tableTypesPrefixed "Row" "Col"
-- myFile.csv@ will generate column types like @ColName@ with a
-- corresponding lens value @colName@ where @Name@ is the name of a
-- column in the CSV file.
tableTypesPrefixed :: String -> String -> FilePath -> DecsQ
tableTypesPrefixed = tableTypesPrefixedOpt [] defaultSep

-- * Customized Data Set Parsing

-- | Generate a type for each row of a table. This will be something
-- like @Rec ["x" :-> a, "y" :-> b, "z" :-> c]@. Possible column types
-- are identified by the given proxy for a type universe. Column type
-- synonyms are /not/ generated (see 'tableTypes').
tableTypeOpt' :: forall proxy a. (ColumnTypeable a, Monoid a)
              => proxy a -> [String] -> String -> String -> FilePath -> DecsQ
tableTypeOpt' _ colNames sep n csvFile =
    pure . TySynD (mkName n) [] <$>
    (runIO (readColHeaders opts csvFile) >>= recDec')
  where recDec' = recDec :: [(T.Text, a)] -> Q Type
        colNames' | null colNames = Nothing
                  | otherwise = Just (map T.pack colNames)
        opts = ParserOptions colNames' (T.pack sep)

-- | Generate a type for each row of a table. This will be something
-- like @Rec ["x" :-> a, "y" :-> b, "z" :-> c]@.
tableTypeOpt :: [String] -> String -> String -> FilePath -> DecsQ
tableTypeOpt = tableTypeOpt' (Proxy::Proxy ColType)

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens (Proxy ::
-- Proxy Foo)@, and @foo' = rlens' (Proxy :: Proxy Foo)@. Possible
-- column types are identified by the given proxy for a type universe.
tableTypesOpt' :: (ColumnTypeable a, Monoid a)
               => proxy a -> [String] -> String -> String -> FilePath -> DecsQ
tableTypesOpt' p colNames sep = flip (tableTypesPrefixedOpt' p colNames sep) ""

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens (Proxy :: Proxy
-- Foo)@, and @foo' = rlens' (Proxy :: Proxy Foo)@.
tableTypesOpt :: [String] -> String -> String -> FilePath -> DecsQ
tableTypesOpt = tableTypesOpt' (Proxy::Proxy ColType)

-- | Like 'tableTypes', but prefixes each column type and proxy value
-- name with the second argument. This is useful if you have very
-- generic column names. For example, @tableTypesPrefixed "Row" "Col"
-- myFile.csv@ will generate column types like @ColName@ with a
-- corresponding lens value @colName@ where @Name@ is the name of a
-- column in the CSV file. Possible column types are identified by the
-- given proxy for a type universe.
tableTypesPrefixedOpt' :: forall proxy a. (ColumnTypeable a, Monoid a)
                       => proxy a  -- ^ Universe of column types
                       -> [String] -- ^ Column names
                       -> String   -- ^ Separator string
                       -> String   -- ^ Row type name
                       -> String   -- ^ Column name prefix
                       -> FilePath -- ^ CSV file
                       -> DecsQ
tableTypesPrefixedOpt' _ colNames sep n prefix csvFile =
  do headers <- runIO $ readColHeaders opts csvFile
     recTy <- tySynD (mkName n) [] (recDec' headers)
     let optsName = case n of
                      [] -> error "Row type name shouldn't be empty"
                      h:t -> mkName $ toLower h : t ++ "Parser"
     optsTy <- sigD optsName [t|ParserOptions|]
     optsDec <- valD (varP optsName) (normalB $ lift opts) []
     colDecs <- concat <$> mapM (uncurry $ colDec (T.pack prefix)) headers
     return (recTy : optsTy : optsDec : colDecs)     
     -- (:) <$> (tySynD (mkName n) [] (recDec' headers))
     --     <*> (concat <$> mapM (uncurry $ colDec (T.pack prefix)) headers)
  where recDec' = recDec :: [(T.Text, a)] -> Q Type
        colNames' | null colNames = Nothing
                  | otherwise = Just (map T.pack colNames)
        opts = ParserOptions colNames' (T.pack sep)

-- | Like 'tableTypes', but prefixes each column type and proxy value
-- name with the second argument. This is useful if you have very
-- generic column names. For example, @tableTypesPrefixed "Row" "Col"
-- myFile.csv@ will generate column types like @ColName@ with a
-- corresponding lens value @colName@ where @Name@ is the name of a
-- column in the CSV file.
tableTypesPrefixedOpt :: [String] -> String
                      -> String -> String
                      -> FilePath -> DecsQ
tableTypesPrefixedOpt = tableTypesPrefixedOpt' (Proxy::Proxy ColType)
