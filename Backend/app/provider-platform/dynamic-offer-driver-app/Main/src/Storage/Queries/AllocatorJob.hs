{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.AllocatorJob where

import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Lib.Scheduler.Environment
import Lib.Scheduler.ScheduleJob
import Lib.Scheduler.Types
import SharedLogic.Allocator
import Storage.Tabular.AllocatorJob

createAllocatorSendSearchRequestToDriverJob :: NominalDiffTime -> SendSearchRequestToDriverJobData -> Esq.SqlDB m ()
createAllocatorSendSearchRequestToDriverJob inTime jobData = do
  void $
    createJobIn @_ @'SendSearchRequestToDriver Storage.Queries.AllocatorJob.create inTime $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

create :: AnyJob AllocatorJobType -> SqlDB m ()
create = Esq.create

findAll :: SchedulerM [AnyJob AllocatorJobType]
findAll = Esq.findAll @SchedulerM @SchedulerM $ from $ table @AllocatorJobT

findById :: Id (AnyJob AllocatorJobType) -> SchedulerM (Maybe (AnyJob AllocatorJobType))
findById = Esq.findById @SchedulerM @SchedulerM

getTasksById :: [Id (AnyJob AllocatorJobType)] -> SchedulerM [AnyJob AllocatorJobType]
getTasksById ids = Esq.findAll @SchedulerM @SchedulerM $ do
  job <- from $ table @AllocatorJobT
  where_ $ job ^. AllocatorJobId `in_` valList (map (.getId) ids)
  pure job

getReadyTasks :: SchedulerM [AnyJob AllocatorJobType]
getReadyTasks = do
  now <- getCurrentTime
  Esq.findAll @SchedulerM @SchedulerM $ do
    job <- from $ table @AllocatorJobT
    where_ $
      job ^. AllocatorJobStatus ==. val Pending
        &&. job ^. AllocatorJobScheduledAt <=. val now
    orderBy [asc $ job ^. AllocatorJobScheduledAt]
    pure job

updateStatus :: JobStatus -> Id (AnyJob AllocatorJobType) -> SchedulerM ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @AllocatorJobT @SchedulerM $ \job -> do
    set job [AllocatorJobStatus =. val newStatus, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

markAsComplete :: Id (AnyJob AllocatorJobType) -> SchedulerM ()
markAsComplete = updateStatus Completed

markAsFailed :: Id (AnyJob AllocatorJobType) -> SchedulerM ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: Id (AnyJob AllocatorJobType) -> Int -> SchedulerM ()
updateErrorCountAndFail jobId fCount = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @AllocatorJobT @SchedulerM $ \job -> do
    set job [AllocatorJobStatus =. val Failed, AllocatorJobCurrErrors =. val fCount, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

reSchedule :: Id (AnyJob AllocatorJobType) -> UTCTime -> SchedulerM ()
reSchedule jobId newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @AllocatorJobT @SchedulerM $ \job -> do
    set job [AllocatorJobScheduledAt =. val newScheduleTime, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

updateFailureCount :: Id (AnyJob AllocatorJobType) -> Int -> SchedulerM ()
updateFailureCount jobId newCountValue = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @AllocatorJobT @SchedulerM $ \job -> do
    set job [AllocatorJobCurrErrors =. val newCountValue, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

reScheduleOnError :: Id (AnyJob AllocatorJobType) -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError jobId newCountValue newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @AllocatorJobT @SchedulerM $ \job -> do
    set
      job
      [ AllocatorJobScheduledAt =. val newScheduleTime,
        AllocatorJobUpdatedAt =. val now,
        AllocatorJobCurrErrors =. val newCountValue
      ]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId
