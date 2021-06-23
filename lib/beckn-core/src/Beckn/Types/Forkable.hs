module Beckn.Types.Forkable where

import Beckn.Types.Flow
import qualified Beckn.Utils.Flow as Flow
import EulerHS.Prelude

class Forkable m where
  fork :: Text -> m () -> m ()
  safeFork ::
    (SomeException -> result) ->
    (success -> result) ->
    Text ->
    (result -> m ()) ->
    m success ->
    m ()

instance Forkable (FlowR r) where
  fork = Flow.fork
  safeFork toError toSuccess name doWithResult f = fork name $ try f >>= doWithResult . either toError toSuccess
