{-# LANGUAGE ConstraintKinds, FlexibleContexts, GADTs, TemplateHaskell,
             TypeOperators #-}

-- | Functions useful for interactively exploring and experimenting
-- with a data set.
module Frames.Exploration (pipePreview, select, lenses, recToList,
                           pr, pr1) where
import Data.Char (isSpace, isUpper)
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl.Functor (Identity(..))
import Frames.Rec
import Frames.RecF (AsVinyl(toVinyl), UnColumn)
import Frames.TypeLevel (AllAre)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
import Pipes.Safe (SafeT, runSafeT, MonadMask)

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

recToList :: (AsVinyl rs, AllAre a (UnColumn rs)) => Record rs -> [a]
recToList = go . toVinyl
  where go :: AllAre a rs => V.Rec Identity rs -> [a]
        go V.RNil = []
        go (Identity x V.:& xs) = x : go xs

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
