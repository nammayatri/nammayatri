module Lib.Scheduler.JobHandler
  ( JobHandlersList,
    emptyJobHandlerList,
    putJobHandlerInList,
    findJobHandlerFunc,
  )
where

import Beckn.Prelude
import Data.Singletons
import Lib.Scheduler.Environment (SchedulerM)
import Lib.Scheduler.Types
import Unsafe.Coerce (unsafeCoerce)

data JobHandler t = forall (e :: t).
  JobTypeConstaints e =>
  JobHandler
  { jobType :: Sing e,
    handlerFunc :: JobHandlerFunc e
  }

type JobHandlerFunc e = Job e -> SchedulerM ExecutionResult

newtype JobHandlersList t = JobHandlersList [JobHandler t]

emptyJobHandlerList :: JobHandlersList t
emptyJobHandlerList = JobHandlersList []

putJobHandlerInList :: forall t (e :: t). (JobTypeConstaints e) => JobHandlerFunc e -> JobHandlersList t -> JobHandlersList t
putJobHandlerInList handler (JobHandlersList map_) = do
  let type_ = sing :: Sing e
  JobHandlersList $ (JobHandler type_ handler :) $ filter (\JobHandler {jobType} -> fromSing jobType /= fromSing type_) map_

findJobHandlerFunc :: forall t (e :: t). (JobTypeConstaints e) => Sing e -> JobHandlersList t -> Maybe (JobHandlerFunc e)
findJobHandlerFunc e (JobHandlersList map_) = getHandlerFunc <$> find (\JobHandler {jobType} -> fromSing jobType == fromSing e) map_
  where
    getHandlerFunc :: JobHandler t -> JobHandlerFunc e
    getHandlerFunc JobHandler {handlerFunc} = unsafeCoerce handlerFunc
