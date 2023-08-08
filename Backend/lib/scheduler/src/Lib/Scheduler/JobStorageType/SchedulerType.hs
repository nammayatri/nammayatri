{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib.Scheduler.JobStorageType.SchedulerType where

import qualified Data.ByteString as BS
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Types.Id
import Kernel.Utils.Time ()
import Lib.Scheduler.Environment
import qualified Lib.Scheduler.JobStorageType.DB.Queries as DBQ
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.Redis.Queries as RQ
import Lib.Scheduler.Types

createJob :: forall t (e :: t) m r. (JobFlow t e, JobMonad m, HedisFlow m r, Esq.EsqDBFlow m r) => Int -> JobContent e -> SchedulerM ()
createJob maxShards jobData = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> void $ RQ.createJob @t @e maxShards jobData
    DbBased -> Esq.runTransaction $ DBQ.createJob @t @e maxShards jobData

createJobIn :: forall t (e :: t) m r. (HasField "schedulerSetName" r Text, JobFlow t e, JobMonad m, Esq.EsqDBFlow m r, HasField "schedulerType" r SchedulerType, HasField "enablePrometheusMetricLogging" r Bool, HasField "enableRedisLatencyLogging" r Bool, HedisFlow m r) => NominalDiffTime -> Int -> JobContent e -> m ()
createJobIn inTime maxShards jobData = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> void $ RQ.createJobIn @t @e inTime maxShards jobData
    DbBased -> Esq.runTransaction $ DBQ.createJobIn @t @e inTime maxShards jobData

createJobByTime :: forall t (e :: t). (JobFlow t e) => UTCTime -> Int -> JobContent e -> SchedulerM ()
createJobByTime byTime maxShards jobData = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> void $ RQ.createJobByTime @t @e byTime maxShards jobData
    DbBased -> Esq.runTransaction $ DBQ.createJobByTime @t @e byTime maxShards jobData

findAll :: (FromTType SchedulerJobT (AnyJob t)) => SchedulerM [AnyJob t]
findAll = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.findAll
    DbBased -> DBQ.findAll

findById :: (FromTType SchedulerJobT (AnyJob t)) => Id AnyJob -> SchedulerM (Maybe (AnyJob t))
findById id = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.findById id
    DbBased -> DBQ.findById id

getTasksById :: (FromTType SchedulerJobT (AnyJob t)) => [Id AnyJob] -> SchedulerM [AnyJob t]
getTasksById jobs = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.getTasksById jobs
    DbBased -> DBQ.getTasksById jobs

getReadyTasks :: (FromJSON (AnyJob t), JobProcessor t) => Maybe Int -> SchedulerM ([AnyJob t], [BS.ByteString])
getReadyTasks mbMaxShards = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.getReadyTasks mbMaxShards
    DbBased -> DBQ.getReadyTasks mbMaxShards

updateStatus :: JobStatus -> Id AnyJob -> SchedulerM ()
updateStatus status id = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.updateStatus status id
    DbBased -> DBQ.updateStatus status id

markAsComplete :: Id AnyJob -> SchedulerM ()
markAsComplete id = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.markAsComplete id
    DbBased -> DBQ.markAsComplete id

markAsFailed :: Id AnyJob -> SchedulerM ()
markAsFailed id = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.markAsFailed id
    DbBased -> DBQ.markAsFailed id

updateErrorCountAndFail :: Id AnyJob -> Int -> SchedulerM ()
updateErrorCountAndFail id errorCount = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.updateErrorCountAndFail id errorCount
    DbBased -> DBQ.updateErrorCountAndFail id errorCount

reSchedule :: Id AnyJob -> UTCTime -> SchedulerM ()
reSchedule id time = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.reSchedule id time
    DbBased -> DBQ.reSchedule id time

updateFailureCount :: Id AnyJob -> Int -> SchedulerM ()
updateFailureCount id failureCount = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.updateFailureCount id failureCount
    DbBased -> DBQ.updateFailureCount id failureCount

reScheduleOnError :: Id AnyJob -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError id newCountValue time = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    RedisBased -> RQ.reScheduleOnError id newCountValue time
    DbBased -> DBQ.reScheduleOnError id newCountValue time
