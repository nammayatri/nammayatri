{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.Scheduler.JobStorageType.Redis.Queries where

import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding as DT
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Utils.App
import Lib.Scheduler.Environment (SchedulerM)
import Lib.Scheduler.JobStorageType.DB.Table
import qualified Lib.Scheduler.ScheduleJob as ScheduleJob
import Lib.Scheduler.Types

createJob :: forall t (e :: t) m r. (JobFlow t e, JobMonad m, HedisFlow m r) => Int -> JobContent e -> m ()
createJob maxShards jobData = do
  void $ ScheduleJob.createJob @t @e createJobFunc maxShards $ JobEntry {jobData = jobData, maxErrors = 5}

createJobIn :: forall t (e :: t) m r. (JobFlow t e, JobMonad m, HedisFlow m r) => NominalDiffTime -> Int -> JobContent e -> m ()
createJobIn inTime maxShards jobData = do
  void $ ScheduleJob.createJobIn @t @e createJobFunc inTime maxShards $ JobEntry {jobData = jobData, maxErrors = 5}

createJobFunc :: (CoreMetrics m, HedisFlow m r) => AnyJob t -> m ()
createJobFunc (AnyJob job) = do
  Hedis.zAdd "Schedule_Job" [(utcTimeToDouble job.scheduledAt, AnyJob job)]

utcTimeToDouble :: UTCTime -> Double
utcTimeToDouble = realToFrac . utcTimeToPOSIXSeconds

createJobByTime :: forall t (e :: t) m. (JobFlow t e, Applicative m) => UTCTime -> Int -> JobContent e -> SchedulerM ()
createJobByTime _ _ _ = pure ()

findAll :: (FromTType SchedulerJobT (AnyJob t)) => SchedulerM ()
findAll = pure ()

findById :: (FromTType SchedulerJobT (AnyJob t)) => Id AnyJob -> SchedulerM ()
findById _ = pure ()

getTasksById :: (FromTType SchedulerJobT (AnyJob t)) => [Id AnyJob] -> SchedulerM [AnyJob t]
getTasksById _ = pure []

getShardIdKey :: Text
getShardIdKey = "DriverOffer:Jobs:ShardId"

getReadyTasks :: (FromJSON (AnyJob t)) => Maybe Int -> SchedulerM [AnyJob t]
getReadyTasks _ = do
  let key :: Text = "Available_Job"
  let groupName :: Text = "myGroup"
  let lastEntryId :: Text = "$"
  mbConsumer <- liftIO getPodName
  let consumerName = fromMaybe "" mbConsumer
  let nextId :: Text = ">"
  isGroupExist <- Hedis.xinfoGroups key
  unless isGroupExist $ do
    _ <- Hedis.xgroupCreate key groupName lastEntryId
    return ()
  result <- Hedis.xreadGroup groupName consumerName [(key, nextId)]
  let textJob = map snd result
  pure $ mapMaybe (DA.decode . BL.fromStrict .DT.encodeUtf8) textJob

updateStatus :: JobStatus -> Id AnyJob -> SchedulerM ()
updateStatus _ _ = pure ()

markAsComplete :: Id AnyJob -> SchedulerM ()
markAsComplete _ = pure ()

markAsFailed :: Id AnyJob -> SchedulerM ()
markAsFailed _ = pure ()

updateErrorCountAndFail :: Id AnyJob -> Int -> SchedulerM ()
updateErrorCountAndFail _ _ = pure ()

reSchedule :: Id AnyJob -> UTCTime -> SchedulerM ()
reSchedule _ _ = pure ()

updateFailureCount :: Id AnyJob -> Int -> SchedulerM ()
updateFailureCount _ _ = pure ()

reScheduleOnError :: Id AnyJob -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError _ _ _ = pure ()
