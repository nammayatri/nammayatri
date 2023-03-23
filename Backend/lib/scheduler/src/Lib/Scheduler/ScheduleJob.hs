{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.ScheduleJob
  ( createJob,
    createJobByTime,
    createJobIn,
    createManyJobs,
  )
where

import Data.Singletons
import qualified Data.UUID as UU
import Kernel.Prelude hiding (mask, throwIO)
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.Types

createJob ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJob createJobFunc maxShards jobEntry = do
  now <- getCurrentTime
  createJobImpl createJobFunc now maxShards jobEntry

createManyJobs ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  ([AnyJob t] -> m ()) ->
  Int ->
  [JobEntry e] ->
  m [Id AnyJob]
createManyJobs createJobFunc maxShards jobs = do
  now <- getCurrentTime
  createManyJobsImpl createJobFunc now maxShards jobs

createJobIn ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  NominalDiffTime ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJobIn createJobFunc diff maxShards jobEntry = do
  now <- getCurrentTime
  when (diff < 0) $ throwError $ InternalError "job can only be scheduled for now or for future"
  let scheduledAt = addUTCTime diff now
  createJobImpl createJobFunc scheduledAt maxShards jobEntry

createJobByTime ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  UTCTime ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJobByTime createJobFunc scheduledAt maxShards jobEntry = do
  now <- getCurrentTime
  when (scheduledAt <= now) $
    throwError $
      InternalError
        "job can only be scheduled for the future\
        \ using createJobByTime, for scheduling for\
        \ now use createJobIn function instead"
  createJobImpl createJobFunc scheduledAt maxShards jobEntry

createManyJobsImpl ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  ([AnyJob t] -> m ()) ->
  UTCTime ->
  Int ->
  [JobEntry e] ->
  m [Id AnyJob]
createManyJobsImpl createManyJobsFunc scheduledAt maxShards entries = do
  now <- getCurrentTime
  jobs <- for entries $ \entry -> do
    id <- generateGUID
    pure $ makeJob maxShards id now scheduledAt entry
  createManyJobsFunc $ fmap AnyJob jobs
  pure $ fmap (.id) jobs

createJobImpl ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  UTCTime ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJobImpl createJobFunc scheduledAt maxShards entry = do
  now <- getCurrentTime
  uuid <- generateGUIDText
  let id = Id uuid
  let job = makeJob maxShards id now scheduledAt entry
  when (job.maxErrors <= 0) $ throwError $ InternalError "maximum errors should be positive"
  createJobFunc $ AnyJob job
  pure id

makeJob ::
  forall t (e :: t).
  (SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  Int ->
  Id AnyJob ->
  UTCTime ->
  UTCTime ->
  JobEntry e ->
  Job e
makeJob maxShards id currentTime scheduledAt JobEntry {..} =
  Job
    { id = id,
      jobInfo = JobInfo (sing :: Sing e) jobData,
      shardId = idToShardNumber . fromJust $ UU.fromText id.getId, -- using fromJust because its never going to fail
      scheduledAt = scheduledAt,
      maxErrors = maxErrors,
      createdAt = currentTime,
      updatedAt = currentTime,
      currErrors = 0,
      status = Pending
    }
  where
    idToShardNumber uuid = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) `mod` maxShards
