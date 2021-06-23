module Beckn.Types.RunSafe where

import Beckn.Types.Flow
import qualified Beckn.Utils.Flow as Flow
import EulerHS.Prelude

class RunSafe m where
  runSafe :: m a -> m (Either Text a)

instance RunSafe (FlowR r) where
  runSafe = Flow.runSafeFlow
