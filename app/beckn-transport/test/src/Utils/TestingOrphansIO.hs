module Utils.TestingOrphansIO where

import Beckn.Utils.Logging (Log (..))
import EulerHS.Prelude

instance Log IO where
  logOutput _logLevel _tags _msg = pure ()
