{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns, CPP, DataKinds, FlexibleInstances,
             KindSignatures, LambdaCase, MultiParamTypeClasses,
             OverloadedStrings, QuasiQuotes, RankNTypes,
             RecordWildCards, ScopedTypeVariables, TemplateHaskell,
             TypeOperators #-}
-- | Infer row types from comma-separated values (CSV) data and read
-- that data from files. Template Haskell is used to generate the
-- necessary types so that you can write type safe programs referring
-- to those types.
module Frames.CSV where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), pure, (<*>))
import Data.Foldable (foldMap)
import Data.Traversable (sequenceA)
import Data.Monoid (Monoid)
#endif

import Control.Arrow (first, second)
import Control.Monad (when, void)
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper)
import qualified Data.Foldable as F
import Data.List (intercalate)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy
import qualified Data.Text as T
import Data.Vinyl (RecMapMethod, rmap, RElem, Rec(..), ElField(..), RecordToList, RMap)
import Data.Vinyl.TypeLevel (RIndex)
import Data.Vinyl.Functor ((:.), Compose(..))
import Frames.Col
import Frames.ColumnTypeable
import Frames.ColumnUniverse
import Frames.Rec
import Frames.RecF
import GHC.TypeLits (KnownSymbol, Symbol)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString
import qualified Pipes.Group
import qualified Pipes.Parse as P
import qualified Pipes.Prelude.Text as PT
import qualified Pipes.Text as PT
import qualified Pipes.Text.Encoding as PT
import qualified Pipes.Safe as P
import qualified Pipes.Safe.Prelude
import System.IO (IOMode(ReadMode))

type Separator = T.Text

type QuoteChar = Char

data QuotingMode
    -- | No quoting enabled. The separator may not appear in values
  = NoQuoting
    -- | Quoted values with the given quoting character. Quotes are escaped by doubling them.
    -- Mostly RFC4180 compliant, except doesn't support newlines in values
  | RFC4180Quoting QuoteChar
  deriving (Eq, Show)

data ParserOptions = ParserOptions { headerOverride :: Maybe [T.Text]
                                   , columnSeparator :: Separator
                                   , quotingMode :: QuotingMode }
  deriving (Eq, Show)

instance Lift QuotingMode where
  lift NoQuoting = [|NoQuoting|]
  lift (RFC4180Quoting char) = [|RFC4180Quoting $(litE . charL $ char)|]

instance Lift ParserOptions where
  lift (ParserOptions Nothing sep quoting) = [|ParserOptions Nothing $sep' $quoting'|]
    where sep' = [|T.pack $(stringE $ T.unpack sep)|]
          quoting' = lift quoting
  lift (ParserOptions (Just hs) sep quoting) = [|ParserOptions (Just $hs') $sep' $quoting'|]
    where sep' = [|T.pack $(stringE $ T.unpack sep)|]
          hs' = [|map T.pack $(listE $  map (stringE . T.unpack) hs)|]
          quoting' = lift quoting

-- | Default 'ParseOptions' get column names from a header line, and
-- use commas to separate columns.
defaultParser :: ParserOptions
defaultParser = ParserOptions Nothing defaultSep (RFC4180Quoting '\"')

-- | Default separator string.
defaultSep :: Separator
defaultSep = T.pack ","

-- * Parsing

-- | Helper to split a 'T.Text' on commas and strip leading and
-- trailing whitespace from each resulting chunk.
tokenizeRow :: ParserOptions -> T.Text -> [T.Text]
tokenizeRow options =
    handleQuoting . T.splitOn sep
  where sep = columnSeparator options
        quoting = quotingMode options
        handleQuoting = case quoting of
          NoQuoting -> id
          RFC4180Quoting quote -> reassembleRFC4180QuotedParts sep quote

-- | Post processing applied to a list of tokens split by the
-- separator which should have quoted sections reassembeld
reassembleRFC4180QuotedParts :: Separator -> QuoteChar -> [T.Text] -> [T.Text]
reassembleRFC4180QuotedParts sep quoteChar = go
  where go [] = []
        go (part:parts)
          | T.null part = T.empty : go parts
          | prefixQuoted part =
            if suffixQuoted part
            then unescape (T.drop 1 . T.dropEnd 1 $ part) : go parts
            else case break suffixQuoted parts of
                   (h,[]) -> [unescape (T.intercalate sep (T.drop 1 part : h))]
                   (h,t:ts) -> unescape
                                 (T.intercalate
                                    sep
                                    (T.drop 1 part : h ++ [T.dropEnd 1 t]))
                               : go ts
          | otherwise = T.strip part : go parts

        prefixQuoted t =
          T.head t == quoteChar--  &&
          -- T.length (T.takeWhile (== quoteChar) t) `rem` 2 == 1

        suffixQuoted t =
          quoteText `T.isSuffixOf` t--  &&
          -- T.length (T.takeWhileEnd (== quoteChar) t) `rem` 2 == 1

        quoteText = T.singleton quoteChar

        unescape :: T.Text -> T.Text
        unescape = T.replace q2 quoteText
          where q2 = quoteText <> quoteText

--tokenizeRow :: Separator -> T.Text -> [T.Text]
--tokenizeRow sep = map (unquote . T.strip) . T.splitOn sep
--  where unquote txt
--          | quoted txt = case T.dropEnd 1 (T.drop 1 txt) of
--                           txt' | T.null txt' -> "Col"
--                                | numish txt' -> txt
--                                | otherwise -> txt'
--          | otherwise = txt
--        numish = T.all (`elem` ("-+.0123456789"::String))
--        quoted txt = case T.uncons txt of
--                       Just ('"', rst)
--                         | not (T.null rst) -> T.last rst == '"'
--                       _ -> False

-- | Infer column types from a prefix (up to 1000 lines) of a CSV
-- file.
prefixInference :: (ColumnTypeable a, Monoid a, Monad m)
                => ParserOptions
                -> P.Parser T.Text m [a]
prefixInference opts = P.draw >>= \case
  Nothing -> return []
  Just row1 -> P.foldAll (\ts -> zipWith (<>) ts . inferCols)
                         (inferCols row1)
                         id
  where inferCols = map inferType . tokenizeRow opts

-- | Extract column names and inferred types from a CSV file.
readColHeaders :: (ColumnTypeable a, Monoid a, Monad m)
               => ParserOptions -> P.Producer T.Text m () -> m [(T.Text, a)]
readColHeaders opts = P.evalStateT $
  do headerRow <- maybe ((tokenizeRow opts
                         . fromMaybe (error "Empty Producer has no header row")) <$> P.draw)
                        pure
                        (headerOverride opts)
     colTypes <- prefixInference opts
     return (zip headerRow colTypes)

-- * Loading Data

-- | Parsing each component of a 'RecF' from a list of text chunks,
-- one chunk per record component.
class ReadRec (rs :: [(Symbol,*)]) where
  readRec :: [T.Text] -> Rec (Either T.Text :. ElField) rs

instance ReadRec '[] where
  readRec _ = RNil

instance (Parseable t, ReadRec ts, KnownSymbol s) => ReadRec (s :-> t ': ts) where
  readRec [] = Compose (Left mempty) :& readRec []
  readRec (h:t) = maybe (Compose (Left (T.copy h)))
                        (Compose . Right . Field)
                        (parse' h) :& readRec t

-- | Produce the lines of a latin1 (or ISO8859 Part 1) encoded file as
-- ’T.Text’ values. Similar to ’PT.readFileLn’ that uses the system
-- locale for decoding, but built on the ’PT.decodeIso8859_1’ decoder.
readFileLatin1Ln :: P.MonadSafe m => FilePath -> P.Producer T.Text m ()
readFileLatin1Ln fp = Pipes.Safe.Prelude.withFile fp ReadMode $ \h ->
  let latinText = void (PT.decodeIso8859_1 (Pipes.ByteString.fromHandle h))
      latinLines = PT.decode PT.lines latinText
  in Pipes.Group.concats latinLines

-- | Read a 'RecF' from one line of CSV.
readRow :: ReadRec rs => ParserOptions -> T.Text -> Rec (Either T.Text :. ElField) rs
readRow = (readRec .) . tokenizeRow

-- | Produce rows where any given entry can fail to parse.
readTableMaybeOpt :: (P.MonadSafe m, ReadRec rs, RMap rs)
                  => ParserOptions
                  -> FilePath
                  -> P.Producer (Rec (Maybe :. ElField) rs) m ()
readTableMaybeOpt opts csvFile =
  PT.readFileLn csvFile >-> pipeTableMaybeOpt opts
{-# INLINE readTableMaybeOpt #-}

-- | Stream lines of CSV data into rows of ’Rec’ values values where
-- any given entry can fail to parse.
pipeTableMaybeOpt :: (Monad m, ReadRec rs, RMap rs)
                  => ParserOptions
                  -> P.Pipe T.Text (Rec (Maybe :. ElField) rs) m ()
pipeTableMaybeOpt opts = do
  when (isNothing (headerOverride opts)) (() <$ P.await)
  P.map (rmap (either (const (Compose Nothing)) (Compose . Just) . getCompose) . readRow opts)
{-# INLINE pipeTableMaybeOpt #-}

-- | Stream lines of CSV data into rows of ’Rec’ values values where
-- any given entry can fail to parse. In the case of a parse failure, the
-- raw 'T.Text' of that entry is retained.
pipeTableEitherOpt :: (Monad m, ReadRec rs)
                   => ParserOptions
                   -> P.Pipe T.Text (Rec (Either T.Text :. ElField) rs) m ()
pipeTableEitherOpt opts = do
  when (isNothing (headerOverride opts)) (() <$ P.await)
  P.map (readRow opts)
{-# INLINE pipeTableEitherOpt #-}

-- | Produce rows where any given entry can fail to parse.
readTableMaybe :: (P.MonadSafe m, ReadRec rs, RMap rs)
               => FilePath -> P.Producer (Rec (Maybe :. ElField) rs) m ()
readTableMaybe = readTableMaybeOpt defaultParser
{-# INLINE readTableMaybe #-}

-- | Stream lines of CSV data into rows of ’Rec’ values where any
-- given entry can fail to parse.
pipeTableMaybe :: (Monad m, ReadRec rs, RMap rs)
               => P.Pipe T.Text (Rec (Maybe :. ElField) rs) m ()
pipeTableMaybe = pipeTableMaybeOpt defaultParser
{-# INLINE pipeTableMaybe #-}

-- | Stream lines of CSV data into rows of ’Rec’ values where any
-- given entry can fail to parse. In the case of a parse failure, the
-- raw 'T.Text' of that entry is retained.
pipeTableEither :: (Monad m, ReadRec rs)
                => P.Pipe T.Text (Rec (Either T.Text :. ElField) rs) m ()
pipeTableEither = pipeTableEitherOpt defaultParser
{-# INLINE pipeTableEither #-}

-- -- | Returns a `MonadPlus` producer of rows for which each column was
-- -- successfully parsed. This is typically slower than 'readTableOpt'.
-- readTableOpt' :: forall m rs.
--                  (MonadPlus m, MonadIO m, ReadRec rs)
--               => ParserOptions -> FilePath -> m (Record rs)
-- readTableOpt' opts csvFile =
--   do h <- liftIO $ do
--             h <- openFile csvFile ReadMode
--             when (isNothing $ headerOverride opts) (void $ T.hGetLine h)
--             return h
--      let go = liftIO (hIsEOF h) >>= \case
--               True -> mzero
--               False -> let r = recMaybe . readRow opts <$> T.hGetLine h
--                        in liftIO r >>= maybe go (flip mplus go . return)
--      go
-- {-# INLINE readTableOpt' #-}

-- -- | Returns a `MonadPlus` producer of rows for which each column was
-- -- successfully parsed. This is typically slower than 'readTable'.
-- readTable' :: forall m rs. (P.MonadSafe m, ReadRec rs)
--            => FilePath -> m (Record rs)
-- readTable' = readTableOpt' defaultParser
-- {-# INLINE readTable' #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTableOpt :: forall m rs.
                (P.MonadSafe m, ReadRec rs, RMap rs)
             => ParserOptions -> FilePath -> P.Producer (Record rs) m ()
readTableOpt opts csvFile = readTableMaybeOpt opts csvFile P.>-> go
  where go = P.await >>= maybe go (\x -> P.yield x >> go) . recMaybe
{-# INLINE readTableOpt #-}

-- | Pipe lines of CSV text into rows for which each column was
-- successfully parsed.
pipeTableOpt :: (ReadRec rs, RMap rs, Monad m)
             => ParserOptions -> P.Pipe T.Text (Record rs) m ()
pipeTableOpt opts = pipeTableMaybeOpt opts >-> P.map recMaybe >-> P.concat
{-# INLINE pipeTableOpt #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTable :: forall m rs. (P.MonadSafe m, ReadRec rs, RMap rs)
          => FilePath -> P.Producer (Record rs) m ()
readTable = readTableOpt defaultParser
{-# INLINE readTable #-}

-- | Pipe lines of CSV text into rows for which each column was
-- successfully parsed.
pipeTable :: (ReadRec rs, RMap rs, Monad m)
          => P.Pipe T.Text (Record rs) m ()
pipeTable = pipeTableOpt defaultParser
{-# INLINE pipeTable #-}

-- * Template Haskell

-- | Generate a column type.
recDec :: [(T.Text, Q Type)] -> Q Type
recDec = appT [t|Record|] . go
  where go [] = return PromotedNilT
        go ((n,t):cs) =
          [t|($(litT $ strTyLit (T.unpack n)) :-> $(t)) ': $(go cs) |]


-- | Capitalize the first letter of a 'T.Text'.
capitalize1 :: T.Text -> T.Text
capitalize1 = foldMap (onHead toUpper) . T.split (not . isAlphaNum)
  where onHead f = maybe mempty (uncurry T.cons . first f) . T.uncons

-- | Massage a column name from a CSV file into a valid Haskell type
-- identifier.
sanitizeTypeName :: T.Text -> T.Text
sanitizeTypeName = unreserved . fixupStart
                 . T.concat . T.split (not . valid) . capitalize1
  where valid c = isAlphaNum c || c == '\'' || c == '_'
        unreserved t
          | t `elem` ["Type", "Class"] = "Col" <> t
          | otherwise = t
        fixupStart t = case T.uncons t of
                         Nothing -> "Col"
                         Just (c,_) | isAlpha c -> t
                                    | otherwise -> "Col" <> t

-- | Declare a type synonym for a column.
mkColTDec :: TypeQ -> Name -> DecQ
mkColTDec colTypeQ colTName = tySynD colTName [] colTypeQ

-- | Declare a singleton value of the given column type and lenses for
-- working with that column.
mkColPDec :: Name -> TypeQ -> T.Text -> DecsQ
mkColPDec colTName colTy colPName = sequenceA [tySig, val, tySig', val']
  where nm = mkName $ T.unpack colPName
        nm' = mkName $ T.unpack colPName <> "'"
        -- tySig = sigD nm [t|Proxy $(conT colTName)|]
        tySig = sigD nm [t|forall f rs. (Functor f,
                            RElem $(conT colTName) rs (RIndex $(conT colTName) rs))
                         => ($colTy -> f $colTy)
                         -> Record rs
                         -> f (Record rs)
                         |]
        tySig' = sigD nm' [t|forall f g rs. (Functor f,
                             RElem $(conT colTName) rs (RIndex $(conT colTName) rs))
                          => (g $(conT colTName) -> f (g $(conT colTName)))
                          -> Rec g rs
                          -> f (Rec g rs)
                          |]
        val = valD (varP nm)
                   (normalB [e|rlens @($(conT colTName)) . rfield |])
                   []
        val' = valD (varP nm')
                    (normalB [e|rlens' @($(conT colTName))|])
                    []

lowerHead :: T.Text -> Maybe T.Text
lowerHead = fmap aux . T.uncons
  where aux (c,t) = T.cons (toLower c) t

-- | For each column, we declare a type synonym for its type, and a
-- Proxy value of that type.
colDec :: ColumnTypeable a => T.Text -> T.Text -> a -> DecsQ
colDec prefix colName colTy = (:) <$> mkColTDec colTypeQ colTName'
                                  <*> mkColPDec colTName' colTyQ colPName
  where colTName = sanitizeTypeName (prefix <> capitalize1 colName)
        colPName = fromMaybe "colDec impossible" (lowerHead colTName)
        colTName' = mkName $ T.unpack colTName
        colTyQ = colType colTy
        colTypeQ = [t|$(litT . strTyLit $ T.unpack colName) :-> $colTyQ|]

-- | Splice for manually declaring a column of a given type. For
-- example, @declareColumn "x2" ''Double@ will declare a type synonym
-- @type X2 = "x2" :-> Double@ and a lens @x2@.
declareColumn :: T.Text -> Name -> DecsQ
declareColumn colName colTy = (:) <$> mkColTDec colTypeQ colTName'
                                  <*> mkColPDec colTName' colTyQ colPName
  where colTName = sanitizeTypeName colName
        colPName = maybe "colDec impossible"
                         (\(c,t) -> T.cons (toLower c) t)
                         (T.uncons colTName)
        colTName' = mkName $ T.unpack colTName
        colTyQ = return (ConT colTy)
        colTypeQ = [t|$(litT . strTyLit $ T.unpack colName) :-> $colTyQ|]

-- * Default CSV Parsing

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
                       , lineReader :: P.Producer T.Text (P.SafeT IO) ()
                       -- ^ A producer of lines of ’T.Text’xs
                       }

-- | Shorthand for a 'Proxy' value of 'ColumnUniverse' applied to the
-- given type list.
colQ :: Name -> Q Exp
colQ n = [e| (Proxy :: Proxy (ColumnUniverse $(conT n))) |]

-- | A default 'RowGen'. This instructs the type inference engine to
-- get column names from the data file, use the default column
-- separator (a comma), infer column types from the default 'Columns'
-- set of types, and produce a row type with name @Row@.
rowGen :: FilePath -> RowGen Columns
rowGen = RowGen [] "" defaultSep "Row" Proxy . PT.readFileLn

-- | Generate a type for each row of a table. This will be something
-- like @Record ["x" :-> a, "y" :-> b, "z" :-> c]@.
tableType :: String -> FilePath -> DecsQ
tableType n fp = tableType' (rowGen fp) { rowTypeName = n }

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens \@Foo@, and
-- @foo' = rlens' \@Foo@.
tableTypes :: String -> FilePath -> DecsQ
tableTypes n fp = tableTypes' (rowGen fp) { rowTypeName = n }

-- * Customized Data Set Parsing

-- | Inspect no more than this many lines when inferring column types.
prefixSize :: Int
prefixSize = 1000

-- | Generate a type for a row of a table. This will be something like
-- @Record ["x" :-> a, "y" :-> b, "z" :-> c]@.  Column type synonyms
-- are /not/ generated (see 'tableTypes'').
tableType' :: forall a. (ColumnTypeable a, Monoid a)
           => RowGen a -> DecsQ
tableType' (RowGen {..}) =
    pure . TySynD (mkName rowTypeName) [] <$>
    (runIO (P.runSafeT (readColHeaders opts lineSource)) >>= recDec')
  where recDec' = recDec . map (second colType) :: [(T.Text, a)] -> Q Type
        colNames' | null columnNames = Nothing
                  | otherwise = Just (map T.pack columnNames)
        opts = ParserOptions colNames' separator (RFC4180Quoting '\"')
        lineSource = lineReader >-> P.take prefixSize

-- | Tokenize the first line of a ’P.Producer’.
colNamesP :: Monad m
          => ParserOptions -> P.Producer T.Text m () -> m [T.Text]
colNamesP opts src = either (const []) (tokenizeRow opts . fst) <$> P.next src

-- | Generate a type for a row of a table all of whose columns remain
-- unparsed 'Text' values.
tableTypesText' :: forall a. (ColumnTypeable a, Monoid a)
                => RowGen a -> DecsQ
tableTypesText' (RowGen {..}) =
  do colNames <- runIO . P.runSafeT $
                 maybe (colNamesP opts lineReader)
                       pure
                       (headerOverride opts)
     let headers = zip colNames (repeat (inferType " "))
     recTy <- tySynD (mkName rowTypeName) [] (recDec' headers)
     let optsName = case rowTypeName of
                      [] -> error "Row type name shouldn't be empty"
                      h:t -> mkName $ toLower h : t ++ "Parser"
     optsTy <- sigD optsName [t|ParserOptions|]
     optsDec <- valD (varP optsName) (normalB $ lift opts) []
     colDecs <- concat <$> mapM (uncurry mkColDecs) headers
     return (recTy : optsTy : optsDec : colDecs)
  where recDec' = recDec . map (second colType) :: [(T.Text, a)] -> Q Type
        colNames' | null columnNames = Nothing
                  | otherwise = Just (map T.pack columnNames)
        opts = ParserOptions colNames' separator (RFC4180Quoting '\"')
        mkColDecs colNm colTy = do
          let safeName = tablePrefix ++ (T.unpack . sanitizeTypeName $ colNm)
          mColNm <- lookupTypeName safeName
          case mColNm of
            Just _ -> pure []
            Nothing -> colDec (T.pack tablePrefix) colNm colTy

-- | Like 'tableType'', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens \@Foo@, and
-- @foo' = rlens' \@Foo@.
tableTypes' :: forall a. (ColumnTypeable a, Monoid a)
            => RowGen a -> DecsQ
tableTypes' (RowGen {..}) =
  do headers <- runIO . P.runSafeT $ readColHeaders opts lineSource
     recTy <- tySynD (mkName rowTypeName) [] (recDec' headers)
     let optsName = case rowTypeName of
                      [] -> error "Row type name shouldn't be empty"
                      h:t -> mkName $ toLower h : t ++ "Parser"
     optsTy <- sigD optsName [t|ParserOptions|]
     optsDec <- valD (varP optsName) (normalB $ lift opts) []
     colDecs <- concat <$> mapM (uncurry mkColDecs) headers
     return (recTy : optsTy : optsDec : colDecs)
     -- (:) <$> (tySynD (mkName n) [] (recDec' headers))
     --     <*> (concat <$> mapM (uncurry $ colDec (T.pack prefix)) headers)
  where recDec' = recDec . map (second colType) :: [(T.Text, a)] -> Q Type
        colNames' | null columnNames = Nothing
                  | otherwise = Just (map T.pack columnNames)
        opts = ParserOptions colNames' separator (RFC4180Quoting '\"')
        lineSource = lineReader >-> P.take prefixSize
        mkColDecs colNm colTy = do
          let safeName = tablePrefix ++ (T.unpack . sanitizeTypeName $ colNm)
          mColNm <- lookupTypeName safeName
          case mColNm of
            Just _ -> pure []
            Nothing -> colDec (T.pack tablePrefix) colNm colTy

-- * Writing CSV Data

-- | 'P.yield' a header row with column names followed by a line of
-- text for each 'Record' with each field separated by a comma. If
-- your source of 'Record' values is a 'P.Producer', consider using
-- 'pipeToCSV' to keep everything streaming.
produceCSV :: forall f ts m.
              (ColumnHeaders ts, Foldable f, Monad m, RecordToList ts,
              RecMapMethod Show ElField ts)
           => f (Record ts) -> P.Producer String m ()
produceCSV recs = do
  P.yield (intercalate "," (columnHeaders (Proxy :: Proxy (Record ts))))
  F.mapM_ (P.yield . intercalate "," . showFields) recs

-- | 'P.yield' a header row with column names followed by a line of
-- text for each 'Record' with each field separated by a comma. This
-- is the same as 'produceCSV', but adapated for cases where you have
-- streaming input that you wish to use to produce streaming output.
pipeToCSV :: forall ts m.
             (Monad m, ColumnHeaders ts, RecordToList ts,
              RecMapMethod Show ElField ts)
          => P.Pipe (Record ts) T.Text m ()
pipeToCSV = P.yield (T.intercalate "," (map T.pack header)) >> go
  where header = columnHeaders (Proxy :: Proxy (Record ts))
        go :: P.Pipe (Record ts) T.Text m ()
        go = P.map (T.intercalate "," . map T.pack . showFields)

-- | Write a header row with column names followed by a line of text
-- for each 'Record' to the given file.
writeCSV :: (ColumnHeaders ts, Foldable f, RecordToList ts,
             RecMapMethod Show ElField ts)
         => FilePath -> f (Record ts) -> IO ()
writeCSV fp recs = P.runSafeT . P.runEffect $
                   produceCSV recs >-> P.map T.pack >-> PT.writeFileLn fp
