module Utils.SilentLogger where

import Beckn.Types.Common
import Beckn.Utils.Logging
import EulerHS.Prelude

instance Log IO where
  logOutput _logLevel _msg = pure ()
  withLogContext _ action = action
