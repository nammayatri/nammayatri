module Beckn.Types.MonadHandler where

import Beckn.Types.Logging
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import EulerHS.Prelude

type MonadHandler m = (MonadCatch m, MonadTime m, MonadGuid m, Log m)
