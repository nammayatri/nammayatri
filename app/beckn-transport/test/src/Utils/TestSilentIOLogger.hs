module Utils.TestSilentIOLogger where

import Beckn.Types.Common
import EulerHS.Prelude

instance Log IO where
  logOutput _logLevel _tags _msg = pure ()
