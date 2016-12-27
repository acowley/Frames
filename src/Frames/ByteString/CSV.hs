{-# LANGUAGE BangPatterns,
             CPP,
             DataKinds,
             FlexibleInstances,
             KindSignatures,
             LambdaCase,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             RecordWildCards,
             ScopedTypeVariables,
             TemplateHaskell,
             TypeOperators #-}
-- | Infer row types from comma-separated values (CSV) data and read
-- that data from files. Template Haskell is used to generate the
-- necessary types so that you can write type safe programs referring
-- to those types.
module Frames.ByteString.CSV where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), pure, (<*>))
import Data.Foldable (foldMap)
import Data.Traversable (sequenceA)
import Data.Monoid (Monoid)
#endif

import Control.Arrow (first, second)
import Control.Monad (MonadPlus(..), when, void)
import Control.Monad.IO.Class
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Search as SS
import Data.Vinyl (RElem, Rec)
import Data.Vinyl.TypeLevel (RIndex)
import Frames.ByteString.Col
import Frames.ByteString.ColumnTypeable
import Frames.ByteString.ColumnUniverse
import Frames.ByteString.Rec
import Frames.ByteString.RecF
import Frames.ByteString.RecLens
import Frames.ByteString.PolyFill as BS
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Pipes as P
import System.IO (Handle, hIsEOF, openFile, IOMode(..), withFile)

type Separator = C8.ByteString

type QuoteChar = Char

data QuotingMode
    -- | No quoting enabled. The separator may not appear in values
  = NoQuoting
    -- | Quoted values with the given quoting character. Quotes are escaped by doubling them.
    -- Mostly RFC4180 compliant, except doesn't support newlines in values
  | RFC4180Quoting QuoteChar
  deriving (Eq, Show)

data ParserOptions = ParserOptions { headerOverride :: Maybe [C8.ByteString]
                                   , columnSeparator :: Separator
                                   , quotingMode :: QuotingMode }
  deriving (Eq, Show)

instance Lift QuotingMode where
  lift NoQuoting = [|NoQuoting|]
  lift (RFC4180Quoting char) = [|RFC4180Quoting $(litE . charL $ char)|]

instance Lift ParserOptions where
  lift (ParserOptions Nothing sep quoting) = [|ParserOptions Nothing $sep' $quoting'|]
    where sep' = [|C8.pack $(stringE $ C8.unpack sep)|]
          quoting' = lift quoting
  lift (ParserOptions (Just hs) sep quoting) = [|ParserOptions (Just $hs') $sep' $quoting'|]
    where sep' = [|C8.pack $(stringE $ C8.unpack sep)|]
          hs' = [|map C8.pack $(listE $  map (stringE . C8.unpack) hs)|]
          quoting' = lift quoting

-- | Default 'ParseOptions' get column names from a header line, and
-- use commas to separate columns.
defaultParser :: ParserOptions
defaultParser = ParserOptions Nothing defaultSep (RFC4180Quoting '\"')

-- | Default separator string.
defaultSep :: Separator
defaultSep = C8.pack ","

-- * Parsing

-- | Helper to split a 'C8.ByteString' on commas and strip leading and
-- trailing whitespace from each resulting chunk.
tokenizeRow :: ParserOptions -> C8.ByteString -> [C8.ByteString]
tokenizeRow options =
    handleQuoting . bsSplitOn sep
  where sep = columnSeparator options
        quoting = quotingMode options
        handleQuoting = case quoting of
          NoQuoting -> id
          RFC4180Quoting quote -> reassembleRFC4180QuotedParts sep quote

-- | Post processing applied to a list of tokens split by the
-- separator which should have quoted sections reassembeld
reassembleRFC4180QuotedParts :: Separator -> QuoteChar -> [C8.ByteString] -> [C8.ByteString]
reassembleRFC4180QuotedParts sep quoteChar = finish . foldr f ([], Nothing)
  where f :: C8.ByteString -> ([C8.ByteString], Maybe C8.ByteString) -> ([C8.ByteString], Maybe C8.ByteString)
        f part (rest, Just accum)
          | prefixQuoted part = let token = unescape (C8.drop 1 part) <> sep <> accum
                                in (token : rest, Nothing)
          | otherwise         = (rest, Just (unescape part <> sep <> accum))
        f part (rest, Nothing)
          | prefixQuoted part &&
            suffixQuoted part = ((unescape . C8.drop 1 . bsDropEnd 1 $ part) : rest, Nothing)
          | suffixQuoted part = (rest, Just (unescape . bsDropEnd 1 $ part))
          | otherwise         = (bsStrip part : rest, Nothing)

        prefixQuoted t =
          quoteText `C8.isPrefixOf` t &&
          (C8.length t - (C8.length . C8.dropWhile (== quoteChar) $ t)) `mod` 2 == 1
        suffixQuoted t =
          quoteText `C8.isSuffixOf` t &&
          (C8.length t - (C8.length . bsDropWhileEnd (== quoteChar) $ t)) `mod` 2 == 1

        quoteText = C8.singleton quoteChar

        unescape :: C8.ByteString -> C8.ByteString
        unescape = LC8.toStrict . (SS.replace (quoteText <> quoteText) quoteText)

        finish :: ([C8.ByteString], Maybe C8.ByteString) -> [C8.ByteString]
        finish (rest, Just dangling) = dangling : rest -- FIXME? just assumes the close quote if it's missing
        finish (rest, Nothing      ) =            rest

--tokenizeRow :: Separator -> C8.ByteString -> [C8.ByteString]
--tokenizeRow sep = map (unquote . C8.strip) . bsSplitOn sep
--  where unquote txt
--          | quoted txt = case bsDropEnd 1 (C8.drop 1 txt) of
--                           txt' | C8.null txt' -> "Col"
--                                | numish txt' -> txt
--                                | otherwise -> txt'
--          | otherwise = txt
--        numish = C8.all (`elem` ("-+.0123456789"::String))
--        quoted txt = case C8.uncons txt of
--                       Just ('"', rst)
--                         | not (C8.null rst) -> C8.last rst == '"'
--                       _ -> False

-- | Infer column types from a prefix (up to 1000 lines) of a CSV
-- file.
prefixInference :: (ColumnTypeable a, Monoid a)
                => ParserOptions -> Handle -> IO [a]
prefixInference opts h = C8.hGetLine h >>= go prefixSize . inferCols
  where prefixSize = 1000 :: Int
        inferCols = map inferType . tokenizeRow opts
        go 0 ts = return ts
        go !n ts =
          hIsEOF h >>= \case
            True -> return ts
            False -> C8.hGetLine h >>= go (n - 1) . zipWith (<>) ts . inferCols

-- -- | Extract column names and inferred types from a CSV file.
readColHeaders :: (ColumnTypeable a, Monoid a)
               => ParserOptions -> FilePath -> IO [(C8.ByteString, a)]
readColHeaders opts f =  withFile f ReadMode $ \h ->
                         zip <$> maybe (tokenizeRow opts <$> C8.hGetLine h)
                                       pure
                                       (headerOverride opts)
                             <*> prefixInference opts h

-- * Loading Data

-- | Parsing each component of a 'RecF' from a list of text chunks,
-- one chunk per record component.
class ReadRec (rs :: [*]) where
  readRec :: [C8.ByteString] -> Rec Maybe rs

instance ReadRec '[] where
  readRec _ = Nil

instance (Parseable t, ReadRec ts) => ReadRec (s :-> t ': ts) where
  readRec [] = frameCons Nothing (readRec [])
  readRec (h:t) = frameCons (parse' h) (readRec t)

-- | Read a 'RecF' from one line of CSV.
readRow :: ReadRec rs => ParserOptions -> C8.ByteString -> Rec Maybe rs
readRow = (readRec .) . tokenizeRow

-- | Produce rows where any given entry can fail to parse.
readTableMaybeOpt :: (MonadIO m, ReadRec rs)
                  => ParserOptions -> FilePath -> P.Producer (Rec Maybe rs) m ()
readTableMaybeOpt opts csvFile =
  do h <- liftIO $ do
            h <- openFile csvFile ReadMode
            when (isNothing $ headerOverride opts) (void $ C8.hGetLine h)
            return h
     let go = liftIO (hIsEOF h) >>= \case
              True -> return ()
              False -> liftIO (readRow opts <$> C8.hGetLine h) >>= P.yield >> go
     go
{-# INLINE readTableMaybeOpt #-}

-- | Produce rows where any given entry can fail to parse.
readTableMaybe :: (MonadIO m, ReadRec rs)
               => FilePath -> P.Producer (Rec Maybe rs) m ()
readTableMaybe = readTableMaybeOpt defaultParser
{-# INLINE readTableMaybe #-}

-- | Returns a `MonadPlus` producer of rows for which each column was
-- successfully parsed. This is typically slower than 'readTableOpt'.
readTableOpt' :: forall m rs.
                 (MonadPlus m, MonadIO m, ReadRec rs)
              => ParserOptions -> FilePath -> m (Record rs)
readTableOpt' opts csvFile =
  do h <- liftIO $ do
            h <- openFile csvFile ReadMode
            when (isNothing $ headerOverride opts) (void $ C8.hGetLine h)
            return h
     let go = liftIO (hIsEOF h) >>= \case
              True -> mzero
              False -> let r = recMaybe . readRow opts <$> C8.hGetLine h
                       in liftIO r >>= maybe go (flip mplus go . return)
     go
{-# INLINE readTableOpt' #-}

-- | Returns a `MonadPlus` producer of rows for which each column was
-- successfully parsed. This is typically slower than 'readTable'.
readTable' :: forall m rs. (MonadPlus m, MonadIO m, ReadRec rs)
           => FilePath -> m (Record rs)
readTable' = readTableOpt' defaultParser
{-# INLINE readTable' #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTableOpt :: forall m rs.
                (MonadIO m, ReadRec rs)
             => ParserOptions -> FilePath -> P.Producer (Record rs) m ()
readTableOpt opts csvFile = readTableMaybeOpt opts csvFile P.>-> go
  where go = P.await >>= maybe go (\x -> P.yield x >> go) . recMaybe
{-# INLINE readTableOpt #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTable :: forall m rs. (MonadIO m, ReadRec rs)
          => FilePath -> P.Producer (Record rs) m ()
readTable = readTableOpt defaultParser
{-# INLINE readTable #-}

-- * Template Haskell

-- | Generate a column type.
recDec :: [(C8.ByteString, Q Type)] -> Q Type
recDec = appT [t|Record|] . go
  where go [] = return PromotedNilT
        go ((n,t):cs) =
          [t|($(litT $ strTyLit (C8.unpack n)) :-> $(t)) ': $(go cs) |]

-- -- | Massage a column name from a CSV file into a valid Haskell type
-- -- identifier.
sanitizeTypeName :: C8.ByteString -> C8.ByteString
sanitizeTypeName = unreserved . fixupStart
                 . C8.concat . C8.splitWith (not . valid) . toTitle'
  where valid c = isAlphaNum c || c == '\'' || c == '_'
        toTitle' = foldMap (onHead toUpper) . C8.splitWith (not . isAlphaNum)
        onHead f = maybe mempty (uncurry C8.cons) . fmap (first f) . C8.uncons
        unreserved t
          | t `elem` ["Type"] = "Col" <> t
          | otherwise = t
        fixupStart t = case C8.uncons t of
                         Nothing -> "Col"
                         Just (c,_) | isAlpha c -> t
                                    | otherwise -> "Col" <> t

-- | Declare a type synonym for a column.
mkColTDec :: TypeQ -> Name -> DecQ
mkColTDec colTypeQ colTName = tySynD colTName [] colTypeQ

-- -- | Declare a singleton value of the given column type and lenses for
-- -- working with that column.
mkColPDec :: Name -> TypeQ -> C8.ByteString -> DecsQ
mkColPDec colTName colTy colPName = sequenceA [tySig, val, tySig', val']
  where nm = mkName $ C8.unpack colPName
        nm' = mkName $ C8.unpack colPName <> "'"
        -- tySig = sigD nm [t|Proxy $(conT colTName)|]
        tySig = sigD nm [t|forall f rs. (Functor f,
                            RElem $(conT colTName) rs (RIndex $(conT colTName) rs))
                         => ($colTy -> f $colTy)
                         -> Record rs
                         -> f (Record rs)
                         |]
        tySig' = sigD nm' [t|forall f g rs. (Functor f, Functor g,
                             RElem $(conT colTName) rs (RIndex $(conT colTName) rs))
                          => (g $(conT colTName) -> f (g $(conT colTName)))
                          -> Rec g rs
                          -> f (Rec g rs)
                          |]
        val = valD (varP nm)
                   (normalB [e|rlens (Proxy :: Proxy $(conT colTName))|])
                   []
        val' = valD (varP nm')
                    (normalB [e|rlens' (Proxy :: Proxy $(conT colTName))|])
                    []

lowerHead :: C8.ByteString -> Maybe C8.ByteString
lowerHead = fmap aux . C8.uncons
  where aux (c,t) = C8.cons (toLower c) t

-- | For each column, we declare a type synonym for its type, and a
-- Proxy value of that type.
colDec :: ColumnTypeable a => C8.ByteString -> C8.ByteString -> a -> DecsQ
colDec prefix colName colTy = (:) <$> mkColTDec colTypeQ colTName'
                                  <*> mkColPDec colTName' colTyQ colPName
  where colTName = sanitizeTypeName (prefix <> colName)
        colPName = fromMaybe "colDec impossible" (lowerHead colTName)
        colTName' = mkName $ C8.unpack colTName
        colTyQ = colType colTy
        colTypeQ = [t|$(litT . strTyLit $ C8.unpack colName) :-> $colTyQ|]

-- | Splice for manually declaring a column of a given type. For
-- example, @declareColumn "x2" ''Double@ will declare a type synonym
-- @type X2 = "x2" :-> Double@ and a lens @x2@.
declareColumn :: C8.ByteString -> Name -> DecsQ
declareColumn colName colTy = (:) <$> mkColTDec colTypeQ colTName'
                                  <*> mkColPDec colTName' colTyQ colPName
  where colTName = sanitizeTypeName colName
        colPName = maybe "colDec impossible"
                         (\(c,t) -> C8.cons (toLower c) t)
                         (C8.uncons colTName)
        colTName' = mkName $ C8.unpack colTName
        colTyQ = return (ConT colTy)
        colTypeQ = [t|$(litT . strTyLit $ C8.unpack colName) :-> $colTyQ|]

-- -- * Default CSV Parsing

-- | Control how row and named column types are generated.
data RowGen a = RowGen { columnNames    :: [String]
                       -- ^ Use these column names. If empty, expect a
                       -- header row in the data file to provide
                       -- column names.
                       , tablePrefix    :: String
                       -- ^ A common prefix to use for every generated
                       -- declaration.
                       , separator      :: Separator
                       -- ^ The string that separates the columns on a
                       -- row.
                       , rowTypeName    :: String
                       -- ^ The row type that enumerates all
                       -- columns.
                       , columnUniverse :: Proxy a
                       -- ^ A type that identifies all the types that
                       -- can be used to classify a column. This is
                       -- essentially a type-level list of types. See
                       -- 'colQ'.
                       }

-- | Shorthand for a 'Proxy' value of 'ColumnUniverse' applied to the
-- given type list.
colQ :: Name -> Q Exp
colQ n = [e| (Proxy :: Proxy (ColumnUniverse $(conT n))) |]

-- | A default 'RowGen'. This instructs the type inference engine to
-- get column names from the data file, use the default column
-- separator (a comma), infer column types from the default 'Columns'
-- set of types, and produce a row type with name @Row@.
rowGen :: RowGen Columns
rowGen = RowGen [] "" defaultSep "Row" Proxy

-- | Generate a type for each row of a table. This will be something
-- like @Record ["x" :-> a, "y" :-> b, "z" :-> c]@.
tableType :: String -> FilePath -> DecsQ
tableType n = tableType' rowGen { rowTypeName = n }

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens (Proxy :: Proxy
-- Foo)@, and @foo' = rlens' (Proxy :: Proxy Foo)@.
tableTypes :: String -> FilePath -> DecsQ
tableTypes n = tableTypes' rowGen { rowTypeName = n }

-- * Customized Data Set Parsing

-- | Generate a type for a row of a table. This will be something like
-- @Record ["x" :-> a, "y" :-> b, "z" :-> c]@.  Column type synonyms
-- are /not/ generated (see 'tableTypes'').
tableType' :: forall a. (ColumnTypeable a, Monoid a)
           => RowGen a -> FilePath -> DecsQ
tableType' (RowGen {..}) csvFile =
    pure . TySynD (mkName rowTypeName) [] <$>
    (runIO (readColHeaders opts csvFile) >>= recDec')
  where recDec' = recDec . map (second colType) :: [(C8.ByteString, a)] -> Q Type
        colNames' | null columnNames = Nothing
                  | otherwise = Just (map C8.pack columnNames)
        opts = ParserOptions colNames' separator (RFC4180Quoting '\"')

-- | Generate a type for a row of a table all of whose columns remain
-- unparsed 'Text' values.
tableTypesText' :: forall a. (ColumnTypeable a, Monoid a)
                => RowGen a -> FilePath -> DecsQ
tableTypesText' (RowGen {..}) csvFile =
  do colNames <- runIO $ withFile csvFile ReadMode $ \h ->
       maybe (tokenizeRow opts <$> C8.hGetLine h)
             pure
             (headerOverride opts)
     let headers = zip colNames (repeat (inferType " "))
     recTy <- tySynD (mkName rowTypeName) [] (recDec' headers)
     let optsName = case rowTypeName of
                      [] -> error "Row type name shouldn't be empty"
                      h:t -> mkName $ toLower h : t ++ "Parser"
     optsTy <- sigD optsName [t|ParserOptions|]
     optsDec <- valD (varP optsName) (normalB $ lift opts) []
     colDecs <- concat <$> mapM (uncurry $ colDec (C8.pack tablePrefix)) headers
     return (recTy : optsTy : optsDec : colDecs)
  where recDec' = recDec . map (second colType) :: [(C8.ByteString, a)] -> Q Type
        colNames' | null columnNames = Nothing
                  | otherwise = Just (map C8.pack columnNames)
        opts = ParserOptions colNames' separator (RFC4180Quoting '\"')

-- | Like 'tableType'', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens (Proxy ::
-- Proxy Foo)@, and @foo' = rlens' (Proxy :: Proxy Foo)@.
tableTypes' :: forall a. (ColumnTypeable a, Monoid a)
            => RowGen a -> FilePath -> DecsQ
tableTypes' (RowGen {..}) csvFile =
  do headers <- runIO $ readColHeaders opts csvFile
     recTy <- tySynD (mkName rowTypeName) [] (recDec' headers)
     let optsName = case rowTypeName of
                      [] -> error "Row type name shouldn't be empty"
                      h:t -> mkName $ toLower h : t ++ "Parser"
     optsTy <- sigD optsName [t|ParserOptions|]
     optsDec <- valD (varP optsName) (normalB $ lift opts) []
     colDecs <- concat <$> mapM (uncurry mkColDecs) headers
     return (recTy : optsTy : optsDec : colDecs)
     -- (:) <$> (tySynD (mkName n) [] (recDec' headers))
     --     <*> (concat <$> mapM (uncurry $ colDec (C8.pack prefix)) headers)
  where recDec' = recDec . map (second colType) :: [(C8.ByteString, a)] -> Q Type
        colNames' | null columnNames = Nothing
                  | otherwise = Just (map C8.pack columnNames)
        opts = ParserOptions colNames' separator (RFC4180Quoting '\"')
        mkColDecs colNm colTy = do
          let safeName = tablePrefix ++ (C8.unpack . sanitizeTypeName $ colNm)
          mColNm <- lookupTypeName safeName
          case mColNm of
            Just _ -> pure []
            Nothing -> colDec (C8.pack tablePrefix) colNm colTy
