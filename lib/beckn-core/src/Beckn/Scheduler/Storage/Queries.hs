module Beckn.Scheduler.Storage.Queries where

import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.Storage.Tabular
import Beckn.Scheduler.Types (Job, JobStatus (COMPLETED, PENDING, TERMINATED), JobText)
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadTime (getCurrentTime))
import Beckn.Types.Id

create :: JobText -> SqlDB ()
create = create'

findAll :: SchedulerM t [JobText]
findAll = Esq.findAll $ from $ table @JobT

findById :: Id JobText -> SchedulerM t (Maybe JobText)
findById = Esq.findById

getTasksById :: [Id JobText] -> SchedulerM t [JobText]
getTasksById ids = Esq.findAll $ do
  job <- from $ table @JobT
  where_ $ job ^. JobId `in_` valList (map (.getId) ids)
  pure job

getReadyTasks :: SchedulerM t [JobText]
getReadyTasks = do
  now <- getCurrentTime
  Esq.findAll $ do
    job <- from $ table @JobT
    where_ $ job ^. JobStatus ==. val PENDING &&. job ^. JobScheduledAt <=. val now
    orderBy [asc $ job ^. JobScheduledAt]
    pure job

updateStatus :: JobStatus -> Id (Job a b) -> SchedulerM t ()
updateStatus newStatus jobId = Esq.update $ \job -> do
  set job [JobStatus =. val newStatus]
  where_ $ job ^. JobId ==. val jobId.getId

markAsComplete :: Id (Job a b) -> SchedulerM t ()
markAsComplete = updateStatus COMPLETED

markAsTerminated :: Id (Job a b) -> SchedulerM t ()
markAsTerminated = updateStatus TERMINATED

updateErrorCountAndTerminate :: Id (Job a b) -> Int -> SchedulerM t ()
updateErrorCountAndTerminate jobId fCount = Esq.update $ \job -> do
  set job [JobStatus =. val TERMINATED, JobCurrErrors =. val fCount]
  where_ $ job ^. JobId ==. val jobId.getId

reSchedule :: Id (Job a b) -> UTCTime -> SchedulerM t ()
reSchedule jobId newScheduleTime = Esq.update $ \job -> do
  set job [JobScheduledAt =. val newScheduleTime]
  where_ $ job ^. JobId ==. val jobId.getId

updateFailureCount :: Id (Job a b) -> Int -> SchedulerM t ()
updateFailureCount jobId newCountValue = Esq.update $ \job -> do
  set job [JobCurrErrors =. val newCountValue]
  where_ $ job ^. JobId ==. val jobId.getId
