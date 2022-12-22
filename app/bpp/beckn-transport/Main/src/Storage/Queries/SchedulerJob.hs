{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.SchedulerJob where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadTime (getCurrentTime))
import Beckn.Types.Id
import Lib.Scheduler.Environment
import Lib.Scheduler.ScheduleJob
import Lib.Scheduler.Types
import SharedLogic.Scheduler
import Storage.Tabular.SchedulerJob

createScheduleRentalRideRequestJob :: UTCTime -> AllocateRentalJobData -> Esq.SqlDB ()
createScheduleRentalRideRequestJob scheduledAt jobData =
  void $
    createJobByTime Storage.Queries.SchedulerJob.create scheduledAt jobEntry
  where
    jobEntry :: JobEntry 'AllocateRental =
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

create :: AnyJob SchedulerJobType -> SqlDB ()
create = Esq.create

findAll :: SchedulerM [AnyJob SchedulerJobType]
findAll = Esq.findAll $ from $ table @SchedulerJobT

findById :: Id (AnyJob SchedulerJobType) -> SchedulerM (Maybe (AnyJob SchedulerJobType))
findById = Esq.findById

getTasksById :: [Id (AnyJob SchedulerJobType)] -> SchedulerM [AnyJob SchedulerJobType]
getTasksById ids = Esq.findAll $ do
  job <- from $ table @SchedulerJobT
  where_ $ job ^. SchedulerJobId `in_` valList (map (.getId) ids)
  pure job

getReadyTasks :: SchedulerM [AnyJob SchedulerJobType]
getReadyTasks = do
  now <- getCurrentTime
  Esq.findAll $ do
    job <- from $ table @SchedulerJobT
    where_ $
      job ^. SchedulerJobStatus ==. val Pending
        &&. job ^. SchedulerJobScheduledAt <=. val now
    orderBy [asc $ job ^. SchedulerJobScheduledAt]
    pure job

updateStatus :: JobStatus -> Id (AnyJob SchedulerJobType) -> SchedulerM ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [SchedulerJobStatus =. val newStatus, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

markAsComplete :: Id (AnyJob SchedulerJobType) -> SchedulerM ()
markAsComplete = updateStatus Completed

markAsFailed :: Id (AnyJob SchedulerJobType) -> SchedulerM ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: Id (AnyJob SchedulerJobType) -> Int -> SchedulerM ()
updateErrorCountAndFail jobId fCount = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [SchedulerJobStatus =. val Failed, SchedulerJobCurrErrors =. val fCount, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

reSchedule :: Id (AnyJob SchedulerJobType) -> UTCTime -> SchedulerM ()
reSchedule jobId newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [SchedulerJobScheduledAt =. val newScheduleTime, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

updateFailureCount :: Id (AnyJob SchedulerJobType) -> Int -> SchedulerM ()
updateFailureCount jobId newCountValue = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [SchedulerJobCurrErrors =. val newCountValue, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

reScheduleOnError :: Id (AnyJob SchedulerJobType) -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError jobId newCountValue newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set
      job
      [ SchedulerJobScheduledAt =. val newScheduleTime,
        SchedulerJobUpdatedAt =. val now,
        SchedulerJobCurrErrors =. val newCountValue
      ]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId
