{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.AllocatorJob where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadTime (getCurrentTime))
import Beckn.Types.Id
import Lib.Scheduler.Environment
import Lib.Scheduler.ScheduleJob
import Lib.Scheduler.Types
import SharedLogic.Allocator
import Storage.Tabular.AllocatorJob

createAllocatorSendSearchRequestToDriverJob :: NominalDiffTime -> SendSearchRequestToDriverJobData -> Esq.SqlDB ()
createAllocatorSendSearchRequestToDriverJob inTime jobData = do
  void $
    createJobIn @_ @'SendSearchRequestToDriver Storage.Queries.AllocatorJob.create inTime $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

create :: AnyJob AllocatorJobType -> SqlDB ()
create = Esq.create

findAll :: SchedulerM [AnyJob AllocatorJobType]
findAll = Esq.findAll $ from $ table @AllocatorJobT

findById :: Id (AnyJob AllocatorJobType) -> SchedulerM (Maybe (AnyJob AllocatorJobType))
findById = Esq.findById

getTasksById :: [Id (AnyJob AllocatorJobType)] -> SchedulerM [AnyJob AllocatorJobType]
getTasksById ids = Esq.findAll $ do
  job <- from $ table @AllocatorJobT
  where_ $ job ^. AllocatorJobId `in_` valList (map (.getId) ids)
  pure job

getReadyTasks :: SchedulerM [AnyJob AllocatorJobType]
getReadyTasks = do
  now <- getCurrentTime
  Esq.findAll $ do
    job <- from $ table @AllocatorJobT
    where_ $
      job ^. AllocatorJobStatus ==. val Pending
        &&. job ^. AllocatorJobScheduledAt <=. val now
    orderBy [asc $ job ^. AllocatorJobScheduledAt]
    pure job

updateStatus :: JobStatus -> Id (AnyJob AllocatorJobType) -> SchedulerM ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [AllocatorJobStatus =. val newStatus, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

markAsComplete :: Id (AnyJob AllocatorJobType) -> SchedulerM ()
markAsComplete = updateStatus Completed

markAsFailed :: Id (AnyJob AllocatorJobType) -> SchedulerM ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: Id (AnyJob AllocatorJobType) -> Int -> SchedulerM ()
updateErrorCountAndFail jobId fCount = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [AllocatorJobStatus =. val Failed, AllocatorJobCurrErrors =. val fCount, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

reSchedule :: Id (AnyJob AllocatorJobType) -> UTCTime -> SchedulerM ()
reSchedule jobId newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [AllocatorJobScheduledAt =. val newScheduleTime, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

updateFailureCount :: Id (AnyJob AllocatorJobType) -> Int -> SchedulerM ()
updateFailureCount jobId newCountValue = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [AllocatorJobCurrErrors =. val newCountValue, AllocatorJobUpdatedAt =. val now]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId

reScheduleOnError :: Id (AnyJob AllocatorJobType) -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError jobId newCountValue newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set
      job
      [ AllocatorJobScheduledAt =. val newScheduleTime,
        AllocatorJobUpdatedAt =. val now,
        AllocatorJobCurrErrors =. val newCountValue
      ]
    where_ $ job ^. AllocatorJobId ==. val jobId.getId
