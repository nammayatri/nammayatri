{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Scheduler.JobStorageType.Redis.Queries where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.Text.Encoding as DT
import qualified Database.Redis as R
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common (logDebug, logError)
import Kernel.Utils.Time (utcToMilliseconds)
import Lib.Scheduler.Environment
import qualified Lib.Scheduler.ScheduleJob as ScheduleJob
import Lib.Scheduler.Types

createJob ::
  forall t (e :: t) m r.
  (JobFlow t e, JobCreator r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  Int ->
  JobContent e ->
  m ()
createJob merchantId merchantOperatingCityId uuid maxShards jobData = do
  void $ ScheduleJob.createJob @t @e merchantId merchantOperatingCityId uuid createJobFunc maxShards $ JobEntry {jobData = jobData, maxErrors = 5}

createJobIn ::
  forall t (e :: t) m r.
  (JobFlow t e, JobCreator r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  NominalDiffTime ->
  Int ->
  JobContent e ->
  m ()
createJobIn merchantId merchantOperatingCityId uuid inTime maxShards jobData = do
  void $ ScheduleJob.createJobIn @t @e merchantId merchantOperatingCityId uuid createJobFunc inTime maxShards $ JobEntry {jobData = jobData, maxErrors = 5}

createJobFunc :: (HedisFlow m r, HasField "schedulerSetName" r Text, HasField "maxShards" r Int) => AnyJob t -> m ()
createJobFunc (AnyJob job) = do
  key <- getShardKey
  Hedis.withNonCriticalCrossAppRedis $ Hedis.zAdd key [(utcToMilliseconds job.scheduledAt, AnyJob job)]

getShardKey :: (HedisFlow m r, HasField "schedulerSetName" r Text, HasField "maxShards" r Int) => m Text
getShardKey = do
  setName <- asks (.schedulerSetName)
  maxShards <- asks (.maxShards)
  myShardId <- (`mod` maxShards) . fromIntegral <$> Hedis.incr getShardIdKey
  return $ setName <> "{" <> show myShardId <> "}"

createJobByTime ::
  forall t (e :: t) m r.
  (JobFlow t e, JobCreator r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  UTCTime ->
  Int ->
  JobContent e ->
  m ()
createJobByTime merchantId merchantOperatingCityId uuid byTime maxShards jobData = do
  void $ ScheduleJob.createJobByTime @t @e merchantId merchantOperatingCityId uuid createJobFunc byTime maxShards $ JobEntry {jobData = jobData, maxErrors = 5}

-----  below functions aren't implemented for redis -------------
findAll :: (JobExecutor r m, JobProcessor t) => m [AnyJob t]
findAll = return []

findById :: (JobExecutor r m, JobProcessor t) => Id AnyJob -> m (Maybe (AnyJob t))
findById _ = pure Nothing

getTasksById :: (JobExecutor r m, JobProcessor t) => [Id AnyJob] -> m [AnyJob t]
getTasksById _ = pure []

getJobByTypeAndScheduleTime :: (JobMonad r m, JobProcessor t) => Text -> UTCTime -> UTCTime -> m [AnyJob t]
getJobByTypeAndScheduleTime _ _ _ = return []

-------------------------------------------------------

getShardIdKey :: Text
getShardIdKey = "DriverOffer:Jobs:ShardId"

getReadyTasks ::
  ( JobExecutor r m,
    JobProcessor t,
    HasField "version" r DeploymentVersion
  ) =>
  Maybe Int ->
  m [(AnyJob t, BS.ByteString)]
getReadyTasks _ = do
  key <- asks (.streamName)
  groupName <- asks (.groupName)
  -- let lastEntryId :: Text = "$"
  version <- asks (.version)
  let consumerName = version.getDeploymentVersion
  let nextId :: Text = ">"
  -- isGroupExist <- Hedis.withNonCriticalCrossAppRedis $ Hedis.xInfoGroups key
  -- unless isGroupExist $ do
  --   Hedis.withNonCriticalCrossAppRedis $ Hedis.xGroupCreate key groupName lastEntryId
  result' <- Hedis.withNonCriticalCrossAppRedis $ Hedis.xReadGroup groupName consumerName [(key, nextId)]
  let result = maybe [] (concatMap (Hedis.extractKeyValuePairs . records)) result'
  let recordIds = maybe [] (concatMap (Hedis.extractRecordIds . records)) result'
  let textJob = map snd result
  let parsedJobs = map (A.eitherDecode . BL.fromStrict . DT.encodeUtf8) textJob
  case sequence parsedJobs of
    Right jobs -> return $ zip jobs recordIds
    Left err -> do
      logDebug $ "error" <> T.pack err
      return []

getReadyTask ::
  ( JobExecutor r m,
    JobProcessor t,
    HasField "consumerName" r Text,
    HasField "block" r Integer,
    HasField "readCount" r Integer
  ) =>
  m [(AnyJob t, BS.ByteString)]
getReadyTask = do
  key <- asks (.streamName)
  groupName <- asks (.groupName)
  -- Stable per-pod consumer name (version-podName), shared by all runner threads so the PEL is
  -- reclaimable across restarts. Built once in Lib.Scheduler.App.runSchedulerService.
  consumerName <- asks (.consumerName)
  let nextId :: Text = ">"
  block <- asks (.block)
  readCount <- asks (.readCount)
  result' <- Hedis.withMasterRedis $ Hedis.xReadGroupOpts groupName consumerName [(key, nextId)] (Just block) (Just readCount)
  let result = maybe [] (concatMap (Hedis.extractKeyValuePairs . records)) result'
  let recordIds = maybe [] (concatMap (Hedis.extractRecordIds . records)) result'
  let textJob = map snd result
  let parsedJobs = map (A.eitherDecode . BL.fromStrict . DT.encodeUtf8) textJob
  case sequence parsedJobs of
    Right jobs -> return $ zip jobs recordIds
    Left err -> do
      logDebug $ "error" <> T.pack err
      return []

updateStatus :: (JobExecutor r m) => JobStatus -> Id AnyJob -> m ()
updateStatus _ _ = pure ()

markAsComplete :: (JobExecutor r m) => Id AnyJob -> m ()
markAsComplete _ = pure ()

markAsFailed :: (JobExecutor r m) => Id AnyJob -> m ()
markAsFailed _ = pure ()

updateErrorCountAndFail :: (JobExecutor r m, Forkable m, CoreMetrics m) => Id AnyJob -> Int -> m ()
updateErrorCountAndFail _ _ = pure ()

updateKey :: AesonKey.Key -> Text -> Value -> Value
updateKey key newString (A.Object obj) =
  let updateKey' (A.String _) = A.String newString
      updateKey' other = other
   in A.Object $ AKM.fromHashMap . HMS.adjust updateKey' key . AKM.toHashMap $ obj
updateKey _ _ other = other

reSchedule :: forall t m r. (JobCreator r m, HasField "schedulerSetName" r Text, JobProcessor t, ToJSON t) => AnyJob t -> UTCTime -> m ()
reSchedule j byTime = do
  let jobJson = toJSON j
  key <- getShardKey
  case toJSON byTime of
    A.String newScheduleTime -> do
      let newJOB = updateKey "scheduledAt" newScheduleTime jobJson
      Hedis.withNonCriticalCrossAppRedis $ Hedis.zAdd key [(utcToMilliseconds byTime, newJOB)]
    jsonTime -> logError $ "got unsupported scheduleTime type: " <> show jsonTime

updateFailureCount :: (JobExecutor r m) => Id AnyJob -> Int -> m ()
updateFailureCount _ _ = pure ()

reScheduleOnError :: forall t m r. (JobCreator r m, HasField "schedulerSetName" r Text, JobProcessor t, ToJSON t) => AnyJob t -> Int -> UTCTime -> m ()
reScheduleOnError j _ = reSchedule j

-- | XGROUP DELCONSUMER — remove a dead consumer from the stream consumer group. Not provided by the
-- shared-kernel Hedis layer, so wrapped here over the raw hedis primitive. Its pending entries must
-- be reclaimed BEFORE calling this (DELCONSUMER drops them from the PEL). Returns the number of
-- messages the consumer still owned, or -1 on error. Uses the cluster connection (hedisMigrationStage
-- is False for all scheduler deployments); callers wrap it in withNonCriticalCrossAppRedis.
xGroupDelConsumer ::
  (HedisFlow m env, TryException m) =>
  Text -> -- stream key
  Text -> -- group name
  Text -> -- consumer name
  m Integer
xGroupDelConsumer key groupName consumerName = do
  eRes <-
    Hedis.runWithPrefixEither key $ \prefKey ->
      R.xgroupDelConsumer prefKey (DT.encodeUtf8 groupName) (DT.encodeUtf8 consumerName)
  case eRes of
    Left err -> do
      logError $ "FAILED_TO_xGroupDelConsumer group=" <> groupName <> " consumer=" <> consumerName <> " err=" <> show err
      pure (-1)
    Right n -> pure n
