{-# LANGUAGE BangPatterns,
             DataKinds,
             FlexibleInstances,
             KindSignatures,
             LambdaCase,
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
import Frames.Col
import Frames.Rec
import Frames.RecF
import Frames.RecLens
import Frames.TypeLevel
import Language.Haskell.TH
import qualified Pipes as P
import System.IO (Handle, hIsEOF, openFile, IOMode(..), withFile)

-- | Helper to split a 'T.Text' on commas and strip leading and
-- trailing whitespace from each resulting chunk.
tokenizeRow :: T.Text -> [T.Text]
tokenizeRow = map T.strip . T.splitOn ","

-- * Column Types

-- | The set of supported column data types.
data ColType = TBool | TInt | TDouble | TText
  deriving (Bounded,Enum,Eq,Ord,Show)

instance Monoid ColType where
  mempty = maxBound
  mappend x y = toEnum $ max (fromEnum x) (fromEnum y)

-- | Syntax for the Haskell type corresponding to a given 'ColType'.
colType :: ColType -> Q Type
colType TBool = [t|Bool|]
colType TInt = [t|Int|]
colType TDouble = [t|Double|]
colType TText = [t|T.Text|]

-- | Mapping from Haskell types to 'ColType's.
class HaskToColType a where
  haskToColType :: proxy a -> ColType
instance HaskToColType Bool where haskToColType _ = TBool
instance HaskToColType Int where haskToColType _ = TInt
instance HaskToColType Double where haskToColType _ = TDouble
instance HaskToColType T.Text where haskToColType _ = TText

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
inferType :: T.Text -> ColType
inferType t = fromMaybe TText . getFirst $
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
prefixInference :: Handle -> IO [ColType]
prefixInference h = T.hGetLine h >>= go prefixSize . map inferType . tokenizeRow
  where prefixSize = 1000 :: Int
        go 0 ts = return ts
        go !n ts =
          hIsEOF h >>= \case
            True -> return ts
            False -> T.hGetLine h >>=
                     go (n - 1) . zipWith (<>) ts . map inferType . tokenizeRow

-- | Extract column names and inferred types from a CSV file.
readColHeaders :: FilePath -> IO [(T.Text, ColType)]
readColHeaders f =  withFile f ReadMode $ \h ->
                      zip <$> (tokenizeRow <$> T.hGetLine h)
                          <*> prefixInference h

-- * Loading Data

-- | Parsing each component of a 'RecF' from a list of text chunks,
-- one chunk per record component.
class ReadRec (rs :: [*]) where
  readRec :: [T.Text] -> RecF Maybe rs

instance ReadRec '[] where
  readRec _ = Nil

instance (Readable t, ReadRec ts) => ReadRec (s :-> t ': ts) where
  readRec [] = Nothing :& readRec []
  readRec (h:t) = fromText h :& readRec t

-- | Read a 'RecF' from one line of CSV.
readRow :: ReadRec rs => T.Text -> RecF Maybe rs
readRow = readRec . tokenizeRow

-- | Produce rows where any given entry can fail to parse.
readTableMaybe :: (MonadIO m, ReadRec rs)
               => FilePath -> P.Producer (RecF Maybe rs) m ()
readTableMaybe csvFile =
  do h <- liftIO $ do
            h <- openFile csvFile ReadMode
            _ <- T.hGetLine h -- drop the header line
            return h
     let go = liftIO (hIsEOF h) >>= \case
              True -> return ()
              False -> liftIO (readRow <$> T.hGetLine h) >>= P.yield >> go
     go
{-# INLINE readTableMaybe #-}

-- | Returns a `MonadPlus` producer of rows for which each column was
-- successfully parsed. This is typically slower than 'readTable'.
readTable' :: forall m rs.
              (MonadPlus m, MonadIO m, ReadRec rs)
           => FilePath -> m (Rec rs)
readTable' csvFile =
  do h <- liftIO $ do
            h <- openFile csvFile ReadMode
            _ <- T.hGetLine h -- drop the header line
            return h
     let go = liftIO (hIsEOF h) >>= \case
              True -> mzero
              False -> let r = recMaybe . readRow <$> T.hGetLine h
                       in liftIO r >>= maybe go (flip mplus go . return)
     go
{-# INLINE readTable' #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readTable :: forall m rs.
              (MonadIO m, ReadRec rs)
           => FilePath -> P.Producer (Rec rs) m ()
readTable csvFile = readTableMaybe csvFile P.>-> go
  where go = P.await >>= maybe go (\x -> P.yield x >> go) . recMaybe
{-# INLINE readTable #-}

-- * Template Haskell

-- | Generate a column type.
recDec :: [(T.Text, ColType)] -> Q Type
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
        tySig' = sigD nm' [t|(Functor f,
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
colDec :: T.Text -> T.Text -> ColType -> DecsQ
colDec prefix colName colTy = (:) <$> mkColTDec colTypeQ colTName'
                                  <*> mkColPDec colTName' colTyQ colPName
  where colTName = sanitizeTypeName (prefix <> colName)
        colPName = case T.uncons colTName of
                     Just (c,t') -> T.cons (toLower c) t'
                     Nothing -> "colDec impossible"
        colTName' = mkName $ T.unpack colTName
        colTyQ = colType colTy
        colTypeQ = [t|$(litT . strTyLit $ T.unpack colName) :-> $colTyQ|]

-- | Generate a type for each row of a table. This will be something
-- like @Rec ["x" :-> a, "y" :-> b, "z" :-> c]@.
tableType :: String -> FilePath -> DecsQ
tableType n csvFile = pure . TySynD (mkName n) [] <$>
                      (runIO (readColHeaders csvFile) >>= recDec)

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens (Proxy :: Proxy
-- Foo)@, and @foo' = rlens' (Proxy :: Proxy Foo)@.
tableTypes :: String -> FilePath -> DecsQ
tableTypes = flip tableTypesPrefixed ""

-- | Like 'tableTypes', but prefixes each column type and proxy value
-- name with the second argument. This is useful if you have very
-- generic column names. For example, @tableTypesPrefixed "Row" "Col"
-- myFile.csv@ will generate column types like @ColName@ with a
-- corresponding lens value @colName@ where @Name@ is the name of a
-- column in the CSV file.
tableTypesPrefixed :: String -> String -> FilePath -> DecsQ
tableTypesPrefixed n prefix csvFile =
  do headers <- runIO $ readColHeaders csvFile
     (:) <$> (tySynD (mkName n) [] (recDec headers))
         <*> (concat <$> mapM (uncurry $ colDec (T.pack prefix)) headers)
