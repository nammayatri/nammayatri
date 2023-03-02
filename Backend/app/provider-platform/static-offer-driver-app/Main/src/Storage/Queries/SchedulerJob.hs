{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.SchedulerJob where

import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Lib.Scheduler.Environment
import Lib.Scheduler.ScheduleJob
import Lib.Scheduler.Types
import SharedLogic.Scheduler
import Storage.Tabular.SchedulerJob

createScheduleRentalRideRequestJob :: UTCTime -> AllocateRentalJobData -> Esq.SqlDB m ()
createScheduleRentalRideRequestJob scheduledAt jobData =
  void $
    createJobByTime Storage.Queries.SchedulerJob.create scheduledAt jobEntry
  where
    jobEntry :: JobEntry 'AllocateRental =
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

create :: AnyJob SchedulerJobType -> SqlDB m ()
create = Esq.create

findAll :: SchedulerM [AnyJob SchedulerJobType]
findAll = Esq.findAll @SchedulerM @SchedulerM $ from $ table @SchedulerJobT

findById :: Id (AnyJob SchedulerJobType) -> SchedulerM (Maybe (AnyJob SchedulerJobType))
findById = Esq.findById @SchedulerM @SchedulerM

getTasksById :: [Id (AnyJob SchedulerJobType)] -> SchedulerM [AnyJob SchedulerJobType]
getTasksById ids = Esq.findAll @SchedulerM @SchedulerM $ do
  job <- from $ table @SchedulerJobT
  where_ $ job ^. SchedulerJobId `in_` valList (map (.getId) ids)
  pure job

getReadyTasks :: SchedulerM [AnyJob SchedulerJobType]
getReadyTasks = do
  now <- getCurrentTime
  Esq.findAll @SchedulerM @SchedulerM $ do
    job <- from $ table @SchedulerJobT
    where_ $
      job ^. SchedulerJobStatus ==. val Pending
        &&. job ^. SchedulerJobScheduledAt <=. val now
    orderBy [asc $ job ^. SchedulerJobScheduledAt]
    pure job

updateStatus :: JobStatus -> Id (AnyJob SchedulerJobType) -> SchedulerM ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @SchedulerJobT @SchedulerM $ \job -> do
    set job [SchedulerJobStatus =. val newStatus, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

markAsComplete :: Id (AnyJob SchedulerJobType) -> SchedulerM ()
markAsComplete = updateStatus Completed

markAsFailed :: Id (AnyJob SchedulerJobType) -> SchedulerM ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: Id (AnyJob SchedulerJobType) -> Int -> SchedulerM ()
updateErrorCountAndFail jobId fCount = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @SchedulerJobT @SchedulerM $ \job -> do
    set job [SchedulerJobStatus =. val Failed, SchedulerJobCurrErrors =. val fCount, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

reSchedule :: Id (AnyJob SchedulerJobType) -> UTCTime -> SchedulerM ()
reSchedule jobId newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @SchedulerJobT @SchedulerM $ \job -> do
    set job [SchedulerJobScheduledAt =. val newScheduleTime, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

updateFailureCount :: Id (AnyJob SchedulerJobType) -> Int -> SchedulerM ()
updateFailureCount jobId newCountValue = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @SchedulerJobT @SchedulerM $ \job -> do
    set job [SchedulerJobCurrErrors =. val newCountValue, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

reScheduleOnError :: Id (AnyJob SchedulerJobType) -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError jobId newCountValue newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update @SchedulerJobT @SchedulerM $ \job -> do
    set
      job
      [ SchedulerJobScheduledAt =. val newScheduleTime,
        SchedulerJobUpdatedAt =. val now,
        SchedulerJobCurrErrors =. val newCountValue
      ]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId
