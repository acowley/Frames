module Temp where
import Language.Haskell.TH
import System.Directory
import System.IO (hClose)
import System.IO.Temp (openTempFile)

withTempContents :: String -> (FilePath -> Q r) -> Q r
withTempContents contents f =
  do fp <- runIO $ do dir <- getTemporaryDirectory  
                      (fp,h) <- openTempFile dir "FramesSpec"
                      hClose h
                      fp <$ writeFile fp contents
     f fp <* runIO (removeFile fp)
