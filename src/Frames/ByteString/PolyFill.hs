{-# LANGUAGE OverloadedStrings #-}
module Frames.ByteString.PolyFill where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as SS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.Applicative (empty)

bsSplitOn :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
bsSplitOn = SS.split

-- TODO this is SLOW! FIX!
bsDropWhileEnd :: (Char -> Bool) -> BS.ByteString -> BS.ByteString
bsDropWhileEnd p bs = TE.encodeUtf8 $ T.dropWhileEnd p (TE.decodeUtf8 bs)

bsDropEnd :: Int -> BS.ByteString -> BS.ByteString
bsDropEnd n bs = fst (BS.splitAt n bs)

-- TODO this is slow because bsDropWhileEnd is slow
bsDropAround :: (Char -> Bool) -> BS.ByteString -> BS.ByteString
bsDropAround p = C8.dropWhile p . bsDropWhileEnd p

-- TODO this is SLOW! FIX!
bsStrip :: BS.ByteString -> BS.ByteString
bsStrip bs = TE.encodeUtf8 (T.strip (TE.decodeUtf8 bs))
