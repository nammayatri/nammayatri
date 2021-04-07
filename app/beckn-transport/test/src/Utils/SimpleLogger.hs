module Utils.SimpleLogger where

import Beckn.Types.Common
import Beckn.Utils.Logging
import EulerHS.Prelude
import System.IO (hFlush)

instance Log IO where
  logOutput _ msg = do
    putStrLn msg
    hFlush stdout
  withLogContext _ action = action
