{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.CacheFlow
import Kernel.Types.Error
import Kernel.Types.Flow ()
import Kernel.Types.Id
import Kernel.Types.MonadGuid
import Kernel.Utils.Common (Forkable (fork), GuidLike (generateGUID), Milliseconds (Milliseconds), MonadTime (getCurrentTime), fromMaybeM, getCurrentTimestamp, logDebug, logError, threadDelayMilliSec)
import Kernel.Utils.Time ()
import Lib.Scheduler.JobStorageType.DB.Queries (getPendingStuckJobs)
import qualified Lib.Scheduler.JobStorageType.DB.Table as BeamST hiding (Id)
import Lib.Scheduler.Types as ST
import qualified Sequelize as Se
import SharedLogic.Allocator

producerLockKey :: Text
producerLockKey = "Producer:Lock:key"

reviverLockKey :: Text
reviverLockKey = "Reviver:Lock:Key"

runProducer :: Flow ()
runProducer = do
  Hedis.whenWithLockRedis producerLockKey 10 $ do
    someErr <-
      try @_ @SomeException $ do
        begTime <- getCurrentTimestamp
        producerTimestampKey <- asks (.producerTimestampKey)
        startTime <- getTime begTime producerTimestampKey
        endTime <- getCurrentTimestamp
        setName <- asks (.schedulerSetName)
        currentJobs <- Hedis.withNonCriticalCrossAppRedis $ Hedis.zRangeByScore setName startTime endTime
        logDebug $ "Jobs taken out of sortedset : " <> show currentJobs
        logDebug $ "Job chunks produced to be inserted into stream : " <> show currentJobs
        Hedis.set producerTimestampKey endTime
        result <- insertIntoStream currentJobs
        logDebug $ "Jobs inserted into stream with timeStamp :" <> show result
        _ <- Hedis.withNonCriticalCrossAppRedis $ Hedis.zRemRangeByScore setName startTime endTime
        endTime <- getCurrentTimestamp
        let diff = endTime - begTime
        waitTimeMilliSec <- asks (.waitTimeMilliSec)
        fork "" $ addGenericLatency "producer" $ fromIntegral $ fromEnum diff
        threadDelayMilliSec . Milliseconds $ max 0 (fromEnum (waitTimeMilliSec - diff) :: Int)
    case someErr of
      Left err -> logError $ show err
      Right _ -> pure ()

-- data ReviverHandler t = >
--   { getAllPendingJobs' :: Flow [AnyJob t],
--     restoreAnyJobInfoMain' :: StoredJobInfo -> Maybe (AnyJobInfo t)
--   }
-- TODO : This will be used to make it generic

runReviver :: Flow ()
runReviver = do
  reviverInterval <- asks (.reviverInterval)
  T.UTCTime _ todaysDiffTime <- getCurrentTime
  let secondsTillNow = T.diffTimeToPicoseconds todaysDiffTime `div` 1000000000000
      shouldRunReviver = secondsTillNow `mod` (fromIntegral reviverInterval.getMinutes) == 0
  when shouldRunReviver $ Hedis.whenWithLockRedis reviverLockKey 300 runReviver'
  threadDelayMilliSec 60000

runReviver' :: Flow ()
runReviver' = do
  logDebug "Reviver is Running "
  pendingJobs :: [AnyJob AllocatorJobType] <- getAllPendingJobs
  let jobsIds = map @_ @(Id AnyJob, Id AnyJob) (\(AnyJob Job {..}) -> (id, parentJobId)) pendingJobs
  logDebug $ "Total number of pendingJobs in DB : " <> show (length pendingJobs) <> " Pending (JobsIDs, ParentJobIds) : " <> show jobsIds
  updateStatusOfJobs Revived $ map fst jobsIds
  newJobsToExecute <-
    forM pendingJobs $ \(AnyJob x) -> do
      now <- getCurrentTime
      uuid <- generateGUIDText
      maxShards <- asks (.maxShards)
      let newid :: (Id AnyJob) = Id uuid
      (AnyJobInfo jobInfo) :: AnyJobInfo AllocatorJobType <- fromMaybeM (InvalidRequest "jobInfo could not be parsed") (restoreAnyJobInfoMain $ storeJobInfo x.jobInfo)
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
                status = Pending
              }
      createWithKVScheduler $ AnyJob newJob
      logDebug $ "Job Revived and inserted into DB with parentJobId : " <> show x.id <> " JobId : " <> show newid
      pure (AnyJob newJob)

  let newJobsToExecute_ = map (BSL.toStrict . Ae.encode) newJobsToExecute
  logDebug $ "Job produced to be inserted into stream of reviver: " <> show newJobsToExecute_
  result <- insertIntoStream newJobsToExecute_
  logDebug $ "Job Revived and inserted into stream with timestamp" <> show result

insertIntoStream :: [B.ByteString] -> Flow ()
insertIntoStream jobs = do
  forM_ jobs $ \job -> fork "putting into stream" $ do
    streamName <- asks (.streamName)
    entryId <- asks (.entryId)
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

getTime :: (CacheFlow m r) => Double -> Text -> m Double
getTime begTime producerTimestampKey = do
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
