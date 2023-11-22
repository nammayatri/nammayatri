module Alchemist.Utils where

import Kernel.Prelude hiding (hPutStr)
import System.IO

writeToFile :: FilePath -> String -> IO ()
writeToFile filename content = do
  withFile filename WriteMode $ \handle_ -> do
    hPutStr handle_ content
