{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, GADTs, ScopedTypeVariables,
             TemplateHaskell, TypeOperators #-}

-- | Functions useful for interactively exploring and experimenting
-- with a data set.
module Frames.Exploration (pipePreview, select, lenses, recToList,
                           pr, pr1, showFrame, printFrame,
                           takeRows, dropRows) where
import Data.Char (isSpace, isUpper)
import qualified Data.Foldable as F
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Class.Method as V
import Data.Vinyl.Functor (ElField(Field), Const(..))
import Frames.Rec
import GHC.TypeLits (Symbol)
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import Pipes hiding (Proxy)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Pipes.Safe (SafeT, runSafeT, MonadMask)
import Frames.Frame (Frame(Frame))
import Frames.RecF (columnHeaders, ColumnHeaders)

-- * Preview Results

-- | @preview src n f@ prints out the first @n@ results of piping
-- @src@ through @f@.
pipePreview :: (Show b, MonadIO m, MonadMask m)
            => Producer a (SafeT m) () -> Int -> Pipe a b (SafeT m) () -> m ()
pipePreview src n f = runSafeT . runEffect $ src >-> f >-> P.take n >-> P.print

-- * Column Selection

-- | @select (Proxy::Proxy [A,B,C])@ extracts columns @A@, @B@, and
-- @C@, from a larger record. Note, this is just a way of pinning down
-- the type of a usage of 'V.rcast'.
select :: (fs V.⊆ rs) => proxy fs -> Record rs -> Record fs
select _ = V.rcast

-- | @lenses (Proxy::Proxy [A,B,C])@ provides a lens onto columns @A@,
-- @B@, and @C@. This is just a way of pinning down the type of
-- 'V.rsubset'.
lenses :: (fs V.⊆ rs, Functor f)
       => proxy fs -> (Record fs -> f (Record fs)) -> Record rs -> f (Record rs)
lenses _ = V.rsubset

{-# DEPRECATED select "Use Data.Vinyl.rcast with a type application. " #-}
{-# DEPRECATED lenses "Use Data.Vinyl.rsubset with a type application." #-}

-- * Proxy Syntax

-- | A proxy value quasiquoter; a way of passing types as
-- values. @[pr|T|]@ will splice an expression @Proxy::Proxy T@, while
-- @[pr|A,B,C|]@ will splice in a value of @Proxy :: Proxy
-- [A,B,C]@. If we have a record type with @Name@ and @Age@ among
-- other fields, we can write @select @[pr|Name,Age|]@ for a function
-- that extracts those fields from a larger record.
pr :: QuasiQuoter
pr = QuasiQuoter mkProxy undefined undefined undefined
  where mkProxy s = let ts = map strip $ splitOn ',' s
                        cons = mapM (conT . mkName) ts
                        mkList = foldr (AppT . AppT PromotedConsT) PromotedNilT
                    in case ts of
                         [h@(t:_)]
                             | isUpper t -> [|Proxy::Proxy $(fmap head cons)|]
                             | otherwise -> [|Proxy::Proxy $(varT $ mkName h)|]
                         _ -> [|Proxy::Proxy $(fmap mkList cons)|]

-- | Like 'pr', but takes a single type, which is used to produce a
-- 'Proxy' for a single-element list containing only that type. This
-- is useful for passing a single type to a function that wants a list
-- of types.
pr1 :: QuasiQuoter
pr1 = QuasiQuoter mkProxy undefined undefined undefined
  where mkProxy s = let sing x = AppT (AppT PromotedConsT x) PromotedNilT
                    in case s of
                         t:_
                           | isUpper t ->
                             [|Proxy::Proxy $(fmap sing (conT (mkName s)))|]
                           | otherwise ->
                             [|Proxy::Proxy $(fmap sing (varT $ mkName s))|]
                         _ -> error "Empty string passed to pr1"

-- * ToList

recToList :: forall a (rs :: [(Symbol, Type)]).
             (V.RecMapMethod ((~) a) ElField rs, V.RecordToList rs)
          => Record rs -> [a]
recToList = V.recordToList . V.rmapMethod @((~) a) aux
  where aux :: a ~ V.PayloadType ElField t  => V.ElField t -> Const a t
        aux (Field x) = Const x

-- * Helpers

-- | Split on a delimiter.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = go
  where go [] = []
        go xs = let (h,t) = break (== d) xs
                in case t of
                     [] -> [h]
                     (_:t') -> h : go t'

-- | Remove white space from both ends of a 'String'.
strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace

-- | @takeRows n frame@ produces a new 'Frame' made up of the first
-- @n@ rows of @frame@.
takeRows :: Int -> Frame (Record rs) -> Frame (Record rs)
takeRows n (Frame len rows) = Frame (min n len) rows

-- | @dropRows n frame@ produces a new 'Frame' just like @frame@, but
-- not including its first @n@ rows.
dropRows :: Int -> Frame (Record rs) -> Frame (Record rs)
dropRows n (Frame len rows) = Frame (max 0 (len - n)) (\i -> rows (i + n))

-- | Format a 'Frame' to a 'String'.
showFrame :: forall rs.
  (ColumnHeaders rs, V.RecMapMethod Show ElField rs, V.RecordToList rs)
  => String -- ^ Separator between fields
  -> Frame (Record rs) -- ^ The 'Frame' to be formatted to a 'String'
  -> String
showFrame sep frame =
  unlines (intercalate sep (columnHeaders (Proxy :: Proxy (Record rs))) : rows)
  where rows = P.toList (F.mapM_ (P.yield . intercalate sep . showFields) frame)

-- | Print a 'Frame' to 'System.IO.stdout'.
printFrame :: forall rs.
  (ColumnHeaders rs, V.RecMapMethod Show ElField rs, V.RecordToList rs)
  => String -- ^ Separator between fields
  -> Frame (Record rs) -- ^ The 'Frame' to be printed to @stdout@
  -> IO ()
printFrame sep frame = do
  putStrLn (intercalate sep (columnHeaders (Proxy :: Proxy (Record rs))))
  P.runEffect (rows >-> P.stdoutLn)
  where rows = F.mapM_ (P.yield . intercalate sep . showFields) frame
