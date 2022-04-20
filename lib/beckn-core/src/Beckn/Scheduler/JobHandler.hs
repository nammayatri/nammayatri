module Beckn.Scheduler.JobHandler where

import Beckn.Prelude
import Beckn.Scheduler.Serialization
import Beckn.Scheduler.Types
import qualified Control.Monad.Catch as C

data JobHandler m t = forall d.
  JobDataSerializable d =>
  JobHandler
  { handlerFunc :: Job t d -> m ExecutionResult,
    errorCatchers :: Job t d -> [C.Handler m ExecutionResult]
  }

transformErrorHandler :: (forall a. m a -> n a) -> C.Handler m b -> C.Handler n b
transformErrorHandler trans (C.Handler actionM) = C.Handler $ trans . actionM

transformJobHandler :: (forall q. m q -> n q) -> JobHandler m t -> JobHandler n t
transformJobHandler transformFunc (JobHandler handlerFunc_ errorCatchers_) =
  JobHandler
    { handlerFunc = transformFunc . handlerFunc_,
      errorCatchers = map (transformErrorHandler transformFunc) . errorCatchers_
    }

type JobHandlerList m t = [(t, JobHandler m t)]
