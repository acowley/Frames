{-# LANGUAGE CPP, DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             LambdaCase, OverloadedStrings, RankNTypes,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeOperators #-}
-- | Infer row types from comma-separated values (CSV) data and read
-- that data from files. Template Haskell is used to generate the
-- necessary types so that you can write type safe programs referring
-- to those types.
module Frames.CSV where
import Control.Exception (try, IOException)
import Control.Monad (when, unless)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import Data.List (intercalate)
import Data.Maybe (isNothing, fromMaybe)
#if __GLASGOW_HASKELL__ < 808
import Data.Monoid ((<>))
#endif
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Vinyl (recordToList, Rec(..), ElField(..), RecordToList)
import Data.Vinyl (RecMapMethod, rmapMethod, RMap, rmap)
import Data.Vinyl.Class.Method (PayloadType)
import Data.Vinyl.Functor (Const(..), (:.), Compose(..))
import Frames.Col
import Frames.ColumnTypeable
import Frames.Rec
import Frames.RecF
import Frames.ShowCSV
import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as P
import qualified Pipes.Safe as P
import qualified Pipes.Safe.Prelude as Safe
import System.IO (Handle, IOMode(ReadMode, WriteMode))

-- * Parsing

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
                => P.Parser [T.Text] m [a]
prefixInference = P.draw >>= \case
  Nothing -> return []
  Just row1 -> P.foldAll (\ts -> zipWith (<>) ts . inferCols)
                         (inferCols row1)
                         id
  where inferCols = map inferType

-- | Extract column names and inferred types from a CSV file.
readColHeaders :: (ColumnTypeable a, Monoid a, Monad m)
               => ParserOptions -> P.Producer [T.Text] m () -> m [(T.Text, a)]
readColHeaders opts = P.evalStateT $
  do headerRow <- maybe (fromMaybe err <$> P.draw)
                        pure
                        (headerOverride opts)
     colTypes <- prefixInference
     unless (length headerRow == length colTypes) (error errNumColumns)
     return (zip headerRow colTypes)
  where err = error "Empty Producer has no header row"
        errNumColumns =
          unlines
          [ ""
          , "Error parsing CSV: "
          , "  Number of columns in header differs from number of columns"
          , "  found in the remaining file. This may be due to newlines"
          , "  being present within the data itself (not just separating"
          , "  rows). If support for embedded newlines is required, "
          , "  consider using the Frames-dsv package in conjunction with"
          , "  Frames to make use of a different CSV parser."]

-- * Loading CSV Data

-- | Parsing each component of a 'RecF' from a list of text chunks,
-- one chunk per record component.
class ReadRec rs where
  readRec :: [T.Text] -> Rec (Either T.Text :. ElField) rs

instance ReadRec '[] where
  readRec _ = RNil

instance (Parseable t, ReadRec ts, KnownSymbol s) => ReadRec (s :-> t ': ts) where
  readRec [] = Compose (Left mempty) :& readRec []
  readRec (h:t) = maybe (Compose (Left (T.copy h)))
                        (Compose . Right . Field)
                        (parse' h) :& readRec t

-- | Opens a file (in 'P.MonadSafe') and repeatedly applies the given
-- function to the 'Handle' to obtain lines to yield. Adapted from the
-- moribund pipes-text package.
pipeLines :: P.MonadSafe m
          => (Handle -> IO (Either IOException T.Text))
          -> FilePath
          -> P.Producer T.Text m ()
pipeLines pgetLine fp = Safe.withFile fp ReadMode $ \h ->
  let loop = do txt <- P.liftIO (pgetLine h)
                case txt of
                  Left _e -> return ()
                  Right y -> P.yield y >> loop
  in loop

-- | Produce lines of 'T.Text'.
produceTextLines :: P.MonadSafe m => FilePath -> P.Producer T.Text m ()
produceTextLines = pipeLines (try . T.hGetLine)

-- | Produce lines of tokens that were separated by the given
-- separator.
produceTokens :: P.MonadSafe m
              => FilePath
              -> Separator
              -> P.Producer [T.Text] m ()
produceTokens fp sep = produceTextLines fp >-> P.map tokenize
  where tokenize = tokenizeRow popts
        popts = defaultParser { columnSeparator = sep }

-- | Consume lines of 'T.Text', writing them to a file.
consumeTextLines :: P.MonadSafe m => FilePath -> P.Consumer T.Text m r
consumeTextLines fp = Safe.withFile fp WriteMode $ \h ->
  let loop = P.await >>= P.liftIO . T.hPutStrLn h >> loop
  in loop

-- | Produce the lines of a latin1 (or ISO8859 Part 1) encoded file as
-- ’T.Text’ values.
readFileLatin1Ln :: P.MonadSafe m => FilePath -> P.Producer [T.Text] m ()
readFileLatin1Ln fp = pipeLines (try . fmap T.decodeLatin1 . B8.hGetLine) fp
                      >-> P.map (tokenizeRow defaultParser)

-- | Read a 'RecF' from one line of CSV.
readRow :: ReadRec rs
        => ParserOptions -> T.Text -> Rec (Either T.Text :. ElField) rs
readRow = (readRec .) . tokenizeRow

-- | Produce rows where any given entry can fail to parse.
readTableMaybeOpt :: (P.MonadSafe m, ReadRec rs, RMap rs)
                  => ParserOptions
                  -> FilePath
                  -> P.Producer (Rec (Maybe :. ElField) rs) m ()
readTableMaybeOpt opts csvFile =
  produceTokens csvFile (columnSeparator opts) >-> pipeTableMaybeOpt opts
{-# INLINE readTableMaybeOpt #-}

-- | Stream lines of CSV data into rows of ’Rec’ values values where
-- any given entry can fail to parse.
pipeTableMaybeOpt :: (Monad m, ReadRec rs, RMap rs)
                  => ParserOptions
                  -> P.Pipe [T.Text] (Rec (Maybe :. ElField) rs) m ()
pipeTableMaybeOpt opts = do
  when (isNothing (headerOverride opts)) (() <$ P.await)
  P.map (rmap (either (const (Compose Nothing))
                      (Compose . Just) . getCompose)
         . readRec)
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
               => P.Pipe [T.Text] (Rec (Maybe :. ElField) rs) m ()
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
readTableOpt :: (P.MonadSafe m, ReadRec rs, RMap rs)
             => ParserOptions -> FilePath -> P.Producer (Record rs) m ()
readTableOpt opts csvFile = readTableMaybeOpt opts csvFile P.>-> go
  where go = P.await >>= maybe go (\x -> P.yield x >> go) . recMaybe
{-# INLINE readTableOpt #-}

-- | Pipe lines of CSV text into rows for which each column was
-- successfully parsed.
pipeTableOpt :: (ReadRec rs, RMap rs, Monad m)
             => ParserOptions -> P.Pipe [T.Text] (Record rs) m ()
pipeTableOpt opts = pipeTableMaybeOpt opts >-> P.map recMaybe >-> P.concat
{-# INLINE pipeTableOpt #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTable :: (P.MonadSafe m, ReadRec rs, RMap rs)
          => FilePath -> P.Producer (Record rs) m ()
readTable = readTableOpt defaultParser
{-# INLINE readTable #-}

-- | Pipe lines of CSV text into rows for which each column was
-- successfully parsed.
pipeTable :: (ReadRec rs, RMap rs, Monad m)
          => P.Pipe [T.Text] (Record rs) m ()
pipeTable = pipeTableOpt defaultParser
{-# INLINE pipeTable #-}

-- * Writing CSV Data

showFieldsCSV :: (RecMapMethod ShowCSV ElField ts, RecordToList ts)
              => Record ts -> [T.Text]
showFieldsCSV = recordToList . rmapMethod @ShowCSV aux
  where aux :: (ShowCSV (PayloadType ElField a))
            => ElField a -> Const T.Text a
        aux (Field x) = Const (showCSV x)

-- | 'P.yield' a header row with column names followed by a line of
-- text for each 'Record' with each field separated by a comma. If
-- your source of 'Record' values is a 'P.Producer', consider using
-- 'pipeToCSV' to keep everything streaming.
produceCSV :: forall f ts m.
              (ColumnHeaders ts, Foldable f, Monad m, RecordToList ts,
              RecMapMethod ShowCSV ElField ts)
           => f (Record ts) -> P.Producer String m ()
produceCSV recs = do
  P.yield (intercalate "," (columnHeaders (Proxy :: Proxy (Record ts))))
  F.mapM_ (P.yield . T.unpack . T.intercalate "," . showFieldsCSV) recs

-- | 'P.yield' a header row with column names followed by a line of
-- text for each 'Record' with each field separated by a comma. This
-- is the same as 'produceCSV', but adapated for cases where you have
-- streaming input that you wish to use to produce streaming output.
pipeToCSV :: forall ts m.
             (Monad m, ColumnHeaders ts, RecordToList ts,
              RecMapMethod ShowCSV ElField ts)
          => P.Pipe (Record ts) T.Text m ()
pipeToCSV = P.yield (T.intercalate "," (map T.pack header)) >> go
  where header = columnHeaders (Proxy :: Proxy (Record ts))
        go :: P.Pipe (Record ts) T.Text m ()
        go = P.map (T.intercalate "," . showFieldsCSV)

-- | Write a header row with column names followed by a line of text
-- for each 'Record' to the given file.
writeCSV :: (ColumnHeaders ts, Foldable f, RecordToList ts,
             RecMapMethod ShowCSV ElField ts)
         => FilePath -> f (Record ts) -> IO ()
writeCSV fp recs = P.runSafeT . P.runEffect $
                   produceCSV recs >-> P.map T.pack >-> consumeTextLines fp
