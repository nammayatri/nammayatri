{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.SilentLogger where

import Beckn.Types.Common
import EulerHS.Prelude

instance Log IO where
  logOutput _logLevel _msg = pure ()
  withLogTag _ action = action
