{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.JobHandler
  ( JobHandlersList,
    emptyJobHandlerList,
    putJobHandlerInList,
    findJobHandlerFunc,
  )
where

import Data.Singletons
import Kernel.Prelude
import Lib.Scheduler.Environment (SchedulerM)
import Lib.Scheduler.Types
import Unsafe.Coerce (unsafeCoerce)

data JobHandler t = forall (e :: t).
  (JobProcessor t, JobInfoProcessor e) =>
  JobHandler
  { jobType :: Sing e,
    handlerFunc :: JobHandlerFunc e
  }

type JobHandlerFunc e = Job e -> SchedulerM ExecutionResult

newtype JobHandlersList t = JobHandlersList [JobHandler t]

emptyJobHandlerList :: JobHandlersList t
emptyJobHandlerList = JobHandlersList []

putJobHandlerInList :: forall t (e :: t). (JobProcessor t, JobInfoProcessor e, SingI e) => JobHandlerFunc e -> JobHandlersList t -> JobHandlersList t
putJobHandlerInList handler (JobHandlersList map_) = do
  let type_ = sing :: Sing e
  JobHandlersList $ (JobHandler type_ handler :) $ filter (\JobHandler {jobType} -> fromSing jobType /= fromSing type_) map_

findJobHandlerFunc :: forall t (e :: t). Job e -> JobHandlersList t -> Maybe (JobHandlerFunc e)
findJobHandlerFunc Job {jobInfo} (JobHandlersList map_) = do
  let JobInfo {jobType = jobType_} = jobInfo
  getHandlerFunc <$> find (\JobHandler {jobType} -> fromSing jobType == fromSing jobType_) map_
  where
    getHandlerFunc :: JobHandler t -> JobHandlerFunc e
    -- we expect to find handler with the same as Job.JobInfo.JobType, so unsafeCoerce
    getHandlerFunc JobHandler {handlerFunc} = unsafeCoerce handlerFunc
