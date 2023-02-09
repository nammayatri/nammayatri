{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.SilentLogger where

import EulerHS.Prelude
import Kernel.Types.Common

instance Log IO where
  logOutput _logLevel _msg = pure ()
  withLogTag _ action = action
