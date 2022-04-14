module Beckn.Scheduler.Storage.Queries where

import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.Storage.Tabular
import Beckn.Scheduler.Types (Job, JobStatus (COMPLETED, PENDING, TERMINATED))
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadTime (getCurrentTime))
import Beckn.Types.Id

create :: Job -> SqlDB ()
create = create'

findAll :: SchedulerM [Job]
findAll = Esq.findAll $ from $ table @JobT

getTasksById :: [Id Job] -> SchedulerM [Job]
getTasksById ids = Esq.findAll $ do
  job <- from $ table @JobT
  where_ $ job ^. JobId `in_` valList (map (.getId) ids)
  pure job

getReadyTasks :: SchedulerM [Job]
getReadyTasks = do
  now <- getCurrentTime
  Esq.findAll $ do
    job <- from $ table @JobT
    where_ $ job ^. JobStatus ==. val PENDING &&. job ^. JobScheduledAt <=. val now
    orderBy [asc $ job ^. JobScheduledAt]
    pure job

updateStatus :: JobStatus -> Id Job -> SchedulerM ()
updateStatus newStatus jobId = Esq.update $ \job -> do
  set job [JobStatus =. val newStatus]
  where_ $ job ^. JobId ==. val jobId.getId

markAsComplete :: Id Job -> SchedulerM ()
markAsComplete = updateStatus COMPLETED

markAsTerminated :: Id Job -> SchedulerM ()
markAsTerminated = updateStatus TERMINATED

updateErrorCountAndTerminate :: Id Job -> Int -> SchedulerM ()
updateErrorCountAndTerminate jobId fCount = Esq.update $ \job -> do
  set job [JobStatus =. val TERMINATED, JobCurrErrors =. val fCount]
  where_ $ job ^. JobId ==. val jobId.getId

reSchedule :: Id Job -> UTCTime -> SchedulerM ()
reSchedule jobId newScheduleTime = Esq.update $ \job -> do
  set job [JobScheduledAt =. val newScheduleTime]
  where_ $ job ^. JobId ==. val jobId.getId

updateFailureCount :: Id Job -> Int -> SchedulerM ()
updateFailureCount jobId newCountValue = Esq.update $ \job -> do
  set job [JobCurrErrors =. val newCountValue]
  where_ $ job ^. JobId ==. val jobId.getId
