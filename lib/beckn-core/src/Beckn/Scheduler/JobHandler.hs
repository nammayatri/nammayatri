module Beckn.Scheduler.JobHandler where

import Beckn.Prelude
import Beckn.Scheduler.Types

data JobHandler m t = forall d.
  JobDataConstraints d =>
  JobHandler
  { handlerFunc :: Job t d -> m ExecutionResult
  }

transformJobHandler :: (forall q. m q -> n q) -> JobHandler m t -> JobHandler n t
transformJobHandler transformFunc (JobHandler handlerFunc_) =
  JobHandler
    { handlerFunc = transformFunc . handlerFunc_
    }

type JobHandlerList m t = [(t, JobHandler m t)]
