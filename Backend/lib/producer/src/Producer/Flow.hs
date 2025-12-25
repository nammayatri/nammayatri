{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Producer.Flow where

import qualified Data.Aeson as Ae
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import qualified Data.Time as T hiding (getCurrentTime)
import qualified Data.UUID as UU
import Environment
import Kernel.Beam.Functions (createWithKVScheduler, updateWithKVScheduler)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.CacheFlow
import Kernel.Types.Error
-- import qualified EulerHS.Language as L
import Kernel.Types.Flow ()
import Kernel.Types.Id
import Kernel.Types.MonadGuid
import Kernel.Utils.Common (Forkable (fork), GuidLike (generateGUID), Milliseconds (Milliseconds), MonadTime (getCurrentTime), fromMaybeM, getCurrentTimestamp, logDebug, logError, threadDelayMilliSec, withTryCatch)
import Kernel.Utils.DatastoreLatencyCalculator
import Kernel.Utils.Time ()
import Lib.Scheduler.JobStorageType.DB.Queries (getPendingStuckJobs)
import qualified Lib.Scheduler.JobStorageType.DB.Table as BeamST hiding (Id)
import Lib.Scheduler.Types as ST
import Producer.SchedulerJob ()
import qualified Sequelize as Se
import SharedLogic.Allocator
import "rider-app" SharedLogic.JobScheduler (RiderJobType (..))

producerLockKey :: Text
producerLockKey = "Producer:Lock:key"

reviverLockKey :: Text
reviverLockKey = "Reviver:Lock:Key"

myShardKey :: Text
myShardKey = "MyShard:Key"

getMyShardKey :: Flow Text
getMyShardKey = do
  maxShards <- asks (.maxShards)
  myShardId <- (`mod` maxShards) . fromIntegral <$> Hedis.incr myShardKey
  return $ show myShardId

getShardedKey :: Text -> Text -> Text
getShardedKey key shardId = key <> "{" <> shardId <> "}"

runProducer :: Flow ()
runProducer = do
  myShardId <- getMyShardKey
  Hedis.whenWithLockRedis (getShardedKey producerLockKey myShardId) 10 $ do
    someErr <-
      withTryCatch "produceMessages:producerFlow" $ do
        (_, diff) <- withTimeGeneric "producer" $ do
          producerTimestampKeyPrefix <- asks (.producerTimestampKey)
          let producerTimestampKey = getShardedKey producerTimestampKeyPrefix myShardId
          startTime <- getTime producerTimestampKey
          endTime <- getCurrentTimestamp
          setName <- asks (.schedulerSetName)
          let myShardSetKey = getShardedKey setName myShardId
          currentJobs <- Hedis.withNonCriticalCrossAppRedis $ Hedis.zRangeByScoreByCount myShardSetKey startTime endTime 0 10000 -- 10,000 limit is good enough for a poor pod
          logDebug $ "Job chunks produced to be inserted into stream : " <> show currentJobs
          result <- insertIntoStream currentJobs
          Hedis.set producerTimestampKey endTime
          logDebug $ "Jobs inserted into stream with timeStamp :" <> show result
          Hedis.withNonCriticalCrossAppRedis $ Hedis.zRemRangeByScore myShardSetKey startTime endTime
        waitTimeMilliSec <- asks (.waitTimeMilliSec)
        threadDelayMilliSec . Milliseconds $ max 0 (fromEnum (waitTimeMilliSec - fromIntegral diff) :: Int)
    case someErr of
      Left err -> logError $ show err
      Right _ -> pure ()

runReviver :: ProducerType -> Flow ()
runReviver producerType = do
  reviverInterval <- asks (.reviverInterval)
  T.UTCTime _ todaysDiffTime <- getCurrentTime
  let secondsTillNow = T.diffTimeToPicoseconds todaysDiffTime `div` 1000000000000
      shouldRunReviver = secondsTillNow `mod` (fromIntegral reviverInterval.getMinutes) == 0
  when shouldRunReviver $ Hedis.whenWithLockRedis reviverLockKey 300 (runReviver' producerType)
  threadDelayMilliSec 60000

mkRunningJobKey :: Text -> Text
mkRunningJobKey jobId = "RunnningJob:" <> jobId

runReviver' :: ProducerType -> Flow ()
runReviver' producerType = do
  logDebug "Reviver is Running "
  case producerType of
    Driver -> do
      pendingJobs :: [AnyJob AllocatorJobType] <- getAllPendingJobs
      let jobsIds = map @_ @(Id AnyJob, Id AnyJob) (\(AnyJob Job {..}) -> (id, parentJobId)) pendingJobs
      filteredPendingJobs <- filterM (\(AnyJob Job {..}) -> Hedis.withCrossAppRedis $ Hedis.tryLockRedis (mkRunningJobKey id.getId) 1800) pendingJobs
      logDebug $ "Total number of pendingJobs in DB : " <> show (length filteredPendingJobs) <> " Pending (JobsIDs, ParentJobIds) : " <> show jobsIds
      newJobsToExecute <-
        forM filteredPendingJobs $ \(AnyJob x) -> do
          updateStatusOfJobs Revived [x.id]
          now <- getCurrentTime
          uuid <- generateGUIDText
          maxShards <- asks (.maxShards)
          let newid :: (Id AnyJob) = Id uuid
          (AnyJobInfo jobInfo) :: AnyJobInfo AllocatorJobType <- fromMaybeM (InvalidRequest "driver side jobInfo could not be parsed") (restoreAnyJobInfoMain $ storeJobInfo x.jobInfo)
          let shardId :: Int = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords (fromJust $ UU.fromText uuid))) `mod` maxShards
          let newJob =
                Job
                  { id = newid,
                    parentJobId = x.parentJobId,
                    scheduledAt = now,
                    jobInfo = jobInfo,
                    shardId = shardId,
                    maxErrors = 5,
                    createdAt = now,
                    updatedAt = now,
                    currErrors = 0,
                    status = Pending,
                    merchantId = ST.merchantId x,
                    merchantOperatingCityId = ST.merchantOperatingCityId x
                  }
          createWithKVScheduler $ AnyJob newJob
          logDebug $ "Driver side Job Revived and inserted into DB with parentJobId : " <> show x.id <> " JobId : " <> show newid
          pure (AnyJob newJob)
      let newJobsToExecute_ = map (BSL.toStrict . Ae.encode) newJobsToExecute
      logDebug $ "Job produced to be inserted into stream of reviver: " <> show newJobsToExecute_
      result <- insertIntoStream newJobsToExecute_
      logDebug $ "Job Revived and inserted into stream with timestamp" <> show result
    Rider -> do
      pendingJobs :: [AnyJob RiderJobType] <- getAllPendingJobs
      let jobsIds = map @_ @(Id AnyJob, Id AnyJob) (\(AnyJob Job {..}) -> (id, parentJobId)) pendingJobs
      filteredPendingJobs <- filterM (\(AnyJob Job {..}) -> Hedis.withCrossAppRedis $ Hedis.tryLockRedis (mkRunningJobKey id.getId) 1800) pendingJobs
      logDebug $ "Total number of pendingJobs in DB : " <> show (length filteredPendingJobs) <> " Pending (JobsIDs, ParentJobIds) : " <> show jobsIds
      newJobsToExecute <-
        forM filteredPendingJobs $ \(AnyJob x) -> do
          updateStatusOfJobs Revived [x.id]
          now <- getCurrentTime
          uuid <- generateGUIDText
          maxShards <- asks (.maxShards)
          let newid :: (Id AnyJob) = Id uuid
          (AnyJobInfo jobInfo) :: AnyJobInfo RiderJobType <- fromMaybeM (InvalidRequest "rider side jobInfo could not be parsed") (restoreAnyJobInfoMain $ storeJobInfo x.jobInfo)
          let shardId :: Int = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords (fromJust $ UU.fromText uuid))) `mod` maxShards
          let newJob =
                Job
                  { id = newid,
                    parentJobId = x.parentJobId,
                    scheduledAt = now,
                    jobInfo = jobInfo,
                    shardId = shardId,
                    maxErrors = 5,
                    createdAt = now,
                    updatedAt = now,
                    currErrors = 0,
                    status = Pending,
                    merchantId = ST.merchantId x,
                    merchantOperatingCityId = ST.merchantOperatingCityId x
                  }
          createWithKVScheduler $ AnyJob newJob
          logDebug $ "Rider sideJob Revived and inserted into DB with parentJobId : " <> show x.id <> " JobId : " <> show newid
          pure (AnyJob newJob)
      let newJobsToExecute_ = map (BSL.toStrict . Ae.encode) newJobsToExecute
      logDebug $ "Job produced to be inserted into stream of reviver: " <> show newJobsToExecute_
      result <- insertIntoStream newJobsToExecute_
      logDebug $ "Job Revived and inserted into stream with timestamp" <> show result

insertIntoStream :: [B.ByteString] -> Flow ()
insertIntoStream jobs = do
  streamName <- asks (.streamName)
  entryId <- asks (.entryId)
  forM_ jobs $ \job -> fork "putting into stream" $ do
    eqId <- generateGUID
    let eqIdByteString = TE.encodeUtf8 eqId
    let job_ = job
    let fieldValue = [(eqIdByteString, job_)]
    _ <- Hedis.withNonCriticalCrossAppRedis $ Hedis.xAdd streamName entryId fieldValue
    return ()

splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches _ [] = []
splitIntoBatches batchSize xs =
  let (batch, rest) = splitAt batchSize xs
   in batch : splitIntoBatches batchSize rest

getTime :: (CacheFlow m r) => Text -> m Double
getTime producerTimestampKey = do
  begTime <- getCurrentTimestamp
  Hedis.safeGet producerTimestampKey <&> \case
    Just lastTime -> lastTime
    Nothing -> begTime

getAllPendingJobs :: (JobProcessor t) => Flow [AnyJob t]
getAllPendingJobs = do
  reviveThreshold <- asks (.reviveThreshold)
  currentTime <- getCurrentTime
  let newtime = T.addUTCTime ((-1) * fromIntegral reviveThreshold) currentTime
  getPendingStuckJobs newtime

updateStatusOfJobs :: JobStatus -> [Id AnyJob] -> Flow ()
updateStatusOfJobs newStatus jobIds = do
  now <- getCurrentTime
  updateWithKVScheduler
    [ Se.Set BeamST.status newStatus,
      Se.Set BeamST.updatedAt (T.utcToLocalTime T.utc now)
    ]
    [Se.Is BeamST.id $ Se.In $ getId <$> jobIds]
