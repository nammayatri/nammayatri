{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.SimpleLogger where

import EulerHS.Prelude
import Kernel.Types.Common
import System.IO (hFlush)

instance Log IO where
  logOutput _ msg = do
    putStrLn msg
    hFlush stdout
  withLogTag _ action = action
