{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib.Scheduler.JobStorageType.Redis.Queries where

import qualified Data.Aeson as DA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding as DT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common (logDebug)
import Kernel.Utils.Time (utcToMilliseconds)
import Lib.Scheduler.Environment (SchedulerM)
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.ScheduleJob as ScheduleJob
import Lib.Scheduler.Types

createJob :: forall t (e :: t) m r. (JobFlow t e, JobMonad m, HedisFlow m r, HasField "schedulerSetName" r Text) => Int -> JobContent e -> m ()
createJob maxShards jobData = do
  void $ ScheduleJob.createJob @t @e createJobFunc maxShards $ JobEntry {jobData = jobData, maxErrors = 5}

createJobIn :: forall t (e :: t) m r. (JobFlow t e, JobMonad m, HedisFlow m r, HasField "schedulerSetName" r Text) => NominalDiffTime -> Int -> JobContent e -> m ()
createJobIn inTime maxShards jobData = do
  void $ ScheduleJob.createJobIn @t @e createJobFunc inTime maxShards $ JobEntry {jobData = jobData, maxErrors = 5}

createJobFunc :: (CoreMetrics m, HedisFlow m r, HasField "schedulerSetName" r Text) => AnyJob t -> m ()
createJobFunc (AnyJob job) = do
  key <- asks (.schedulerSetName)
  Hedis.zAdd key [(utcToMilliseconds job.scheduledAt, AnyJob job)]

createJobByTime :: forall t (e :: t). (JobFlow t e) => UTCTime -> Int -> JobContent e -> SchedulerM ()
createJobByTime _ _ _ = pure ()

findAll :: (FromTType SchedulerJobT (AnyJob t)) => SchedulerM [AnyJob t]
findAll = pure []

findById :: (FromTType SchedulerJobT (AnyJob t)) => Id AnyJob -> SchedulerM (Maybe (AnyJob t))
findById _ = pure Nothing

getTasksById :: (FromTType SchedulerJobT (AnyJob t)) => [Id AnyJob] -> SchedulerM [AnyJob t]
getTasksById _ = pure []

getShardIdKey :: Text
getShardIdKey = "DriverOffer:Jobs:ShardId"

getReadyTasks :: (FromJSON (AnyJob t)) => Maybe Int -> SchedulerM ([AnyJob t], [BS.ByteString])
getReadyTasks _ = do
  key <- asks (.streamName)
  groupName <- asks (.groupName)
  let lastEntryId :: Text = "$"
  consumerName <- generateGUIDText
  let nextId :: Text = ">"
  isGroupExist <- Hedis.xInfoGroups key
  unless isGroupExist $ do
    Hedis.xGroupCreate key groupName lastEntryId
  result' <- Hedis.xReadGroup groupName consumerName [(key, nextId)]
  let result = maybe [] (concatMap (Hedis.extractKeyValuePairs . records)) result'
  let recordIds = maybe [] (concatMap (Hedis.extractRecordIds . records)) result'
  let textJob = map snd result
  let parsedJobs = map (DA.eitherDecode . BL.fromStrict . DT.encodeUtf8) textJob
  case sequence parsedJobs of
    Right jobs -> return (jobs, recordIds)
    Left err -> do
      logDebug $ "error" <> T.pack err
      return ([], [])

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
