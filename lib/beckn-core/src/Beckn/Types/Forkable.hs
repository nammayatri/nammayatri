module Beckn.Types.Forkable where

import Beckn.Types.Flow
import qualified Beckn.Utils.Flow as Flow
import EulerHS.Prelude

class Forkable m where
  fork :: Text -> m () -> m ()

instance Forkable (FlowR r) where
  fork = Flow.fork
