{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeOperators #-}

-- | Functions useful for interactively exploring and experimenting
-- with a data set.
module Frames.Exploration (pipePreview, select, lenses, pr) where
import Data.Proxy
import qualified Data.Vinyl as V
import Frames.Rec
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
import Data.Char (isSpace)

-- | @preview src n f@ prints out the first @n@ results of piping
-- @src@ through @f@.
pipePreview :: (MonadIO m, Show b)
            => Producer a m () -> Int -> Pipe a b m () -> m ()
pipePreview src n f = runEffect $ src >-> f >-> P.take n >-> P.print

-- | @select (Proxy::Proxy [A,B,C])@ extracts columns @A@, @B@, and
-- @C@, from a larger record. Note, this is just a way of pinning down
-- the type of a usage of 'V.rcast'.
select :: (fs V.⊆ rs) => proxy fs -> Rec rs -> Rec fs
select _ = V.rcast

-- | @lenses (Proxy::Proxy [A,B,C])@ provides a lens onto columns @A@,
-- @B@, and @C@. This is just a way of pinning down the type of
-- 'V.rsubset'.
lenses :: (fs V.⊆ rs, Functor f)
       => proxy fs -> (Rec fs -> f (Rec fs)) -> Rec rs -> f (Rec rs)
lenses _ = V.rsubset

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

-- | A proxy value quasiquoter. @[pr|T|]@ will splice an expression
-- @Proxy::Proxy T@, while @[pr|A,B,C|]@ will splice in a value of
-- @Proxy :: Proxy [A,B,C]@.
pr :: QuasiQuoter
pr = QuasiQuoter mkProxy undefined undefined undefined
  where mkProxy s = let ts = map strip $ splitOn ',' s
                        cons = mapM (conT . mkName) ts
                        mkList = foldr (AppT . AppT PromotedConsT) PromotedNilT
                    in case ts of
                         [_] -> [|Proxy::Proxy $(fmap head cons)|]
                         _ -> [|Proxy::Proxy $(fmap mkList cons) |]
