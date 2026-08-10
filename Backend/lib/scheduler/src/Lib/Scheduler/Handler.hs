{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Scheduler.Handler
  ( SchedulerHandle (..),
    handler,
    gracefulConsumerCleanup,
  )
where

import Control.Monad.Catch
import qualified Control.Monad.Catch as C
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (partition)
import qualified Data.Map as M
import Data.Singletons (fromSing)
import qualified Data.Text.Encoding as DT
import qualified Data.Time as T hiding (getCurrentTime)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude hiding (mask, throwIO)
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Logging
import Kernel.Tools.LoopGracefully (loopGracefully)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.Time ()
import Lib.Scheduler.Environment
import Lib.Scheduler.JobHandler
import qualified Lib.Scheduler.JobStorageType.Redis.Queries as RQ
import Lib.Scheduler.Types

data SchedulerHandle t = SchedulerHandle
  { jobHandlers :: JobHandlersList t,
    getTasksById :: [Id AnyJob] -> SchedulerM [AnyJob t],
    getReadyTasks :: SchedulerM [(AnyJob t, BS.ByteString)],
    markAsComplete :: Text -> Id AnyJob -> SchedulerM (),
    markAsFailed :: Text -> Id AnyJob -> SchedulerM (),
    getReadyTask :: SchedulerM [(AnyJob t, BS.ByteString)],
    updateErrorCountAndFail :: Text -> Id AnyJob -> Int -> SchedulerM (),
    reSchedule :: Text -> AnyJob t -> UTCTime -> SchedulerM (),
    updateFailureCount :: Text -> Id AnyJob -> Int -> SchedulerM (),
    reScheduleOnError :: Text -> AnyJob t -> Int -> UTCTime -> SchedulerM ()
  }

handler :: forall t. (HasSchemaName BeamSC.SystemConfigsT, JobProcessor t, FromJSON t) => SchedulerHandle t -> SchedulerM ()
handler hnd = do
  schedulerType <- asks (.schedulerType)
  maxThreads <- asks (.maxThreads)
  case schedulerType of
    RedisBased ->
      loopGracefully $
        replicate maxThreads (runnerIterationRedis hnd runTask)
          <> [heartbeatLoop, reclaimerLoop hnd, sweeperLoop hnd]
    DbBased -> loopGracefully $ replicate maxThreads (dbBasedHandlerLoop hnd runTask)
  where
    runTask :: AnyJob t -> SchedulerM ()
    runTask anyJob@(AnyJob Job {..}) = mask $ \restore -> do
      let jobType' = show (fromSing $ jobType jobInfo)
      expirationTime <- asks (.expirationTime)
      withDynamicLogLevel jobType' . Hedis.withCrossAppRedis $ do
        jobInfoMap <- asks (.jobInfoMap)
        let isLongRunningJob = fromMaybe False (M.lookup jobType' jobInfoMap)
            lockFn runFn =
              if isLongRunningJob
                then do
                  gotLock <- Hedis.tryLockRedis (mkRunningJobKey id.getId) 1800
                  when gotLock $ runFn
                else Hedis.whenWithLockRedis (mkRunningJobKey parentJobId.getId) (fromIntegral expirationTime) runFn
        lockFn $
          withLogTag ("JobId = " <> id.getId <> " and parentJobId = " <> parentJobId.getId <> " jobType = " <> jobType' <> " isLongRunningJob = " <> show isLongRunningJob) $ do
            res <- measuringDuration (registerDuration jobType') $ restore (executeTask hnd anyJob) `C.catchAll` defaultCatcher jobType'
            registerExecutionResult hnd anyJob res
            releaseLock parentJobId
            Hedis.unlockRedis (mkRunningJobKey id.getId)
            -- Stream-native retry: requeue happens AFTER the locks are released so the fresh entry
            -- can acquire the parentJobId lock on its next pickup (avoids a lock-held silent drop).
            handleStreamRetry hnd anyJob jobType' res

dbBasedHandlerLoop :: (JobProcessor t, FromJSON t) => SchedulerHandle t -> (AnyJob t -> SchedulerM ()) -> SchedulerM ()
dbBasedHandlerLoop hnd runTask = do
  logInfo "Starting runner iteration 1"
  iterSessionId <- generateGUIDText
  before <- getCurrentTime
  withLogTag iterSessionId $ logInfo "Starting runner iteration"
  runnerIteration hnd runTask
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)

-- dbBasedHandlerLoop hnd runTask

mapConcurrently :: Traversable t => (a -> SchedulerM ()) -> t a -> SchedulerM ()
mapConcurrently action = mapM_ (fork "mapThread" . action)

runnerIterationRedis :: forall t. (JobProcessor t, FromJSON t) => SchedulerHandle t -> (AnyJob t -> SchedulerM ()) -> SchedulerM ()
runnerIterationRedis SchedulerHandle {..} runTask = do
  logInfo "Starting runner iteration"
  key <- asks (.streamName)
  groupName <- asks (.groupName)
  readyTasks <- getReadyTask
  logTagDebug "Available tasks - Count" . show $ length readyTasks
  blackListedJobs <- asks (.blackListedJobs)
  (nonBlacklistedTasks, recordIdsToDelete) <-
    if null blackListedJobs
      then pure (readyTasks, map snd readyTasks) -- No blacklist: execute all, delete all
      else do
        let (blacklistedTasks, nonBlacklistedTasks') = partition (isBlacklisted blackListedJobs) readyTasks
            blacklistedCount = length blacklistedTasks
        when (blacklistedCount > 0) $ do
          logInfo $ "Skipping " <> show blacklistedCount <> " blacklisted job(s)" <> " jobType: " <> show blackListedJobs
          logDebug $ "Blacklisted job IDs: " <> show (map (\(AnyJob Job {..}, _) -> id.getId) blacklistedTasks)
        pure (nonBlacklistedTasks', map snd readyTasks) -- Delete all (both blacklisted and non-blacklisted)
  mapM_ (runTask . fst) nonBlacklistedTasks
  fork "removingFromStream" . unless (null recordIdsToDelete) $ do
    void $ Hedis.withNonCriticalCrossAppRedis $ Hedis.xAck key groupName recordIdsToDelete
    void $ Hedis.withNonCriticalCrossAppRedis $ Hedis.xDel key recordIdsToDelete
  where
    isBlacklisted :: [Text] -> (AnyJob t, BS.ByteString) -> Bool
    isBlacklisted blackList (AnyJob Job {..}, _) =
      let jobType' = show (fromSing $ jobType jobInfo)
       in jobType' `elem` blackList

runnerIteration :: forall t. (JobProcessor t) => SchedulerHandle t -> (AnyJob t -> SchedulerM ()) -> SchedulerM ()
runnerIteration SchedulerHandle {..} runTask = do
  readyJobs <- getReadyTasks
  let readyTasks = map fst readyJobs
  logTagDebug "All Tasks - Count" . show $ length readyTasks
  logTagDebug "All Tasks" . show $ map @_ @(Id AnyJob) (\(AnyJob Job {..}) -> parentJobId) readyTasks
  blackListedJobs <- asks (.blackListedJobs)
  let (blacklistedTasks, nonBlacklistedTasks) =
        if null blackListedJobs
          then ([], readyTasks) -- No blacklist: process all
          else partition (isBlacklistedDB blackListedJobs) readyTasks
  let blacklistedCount = length blacklistedTasks
  when (blacklistedCount > 0) $ do
    logInfo $ "Skipping " <> show blacklistedCount <> " blacklisted job(s) in DB runner" <> " jobType: " <> show blackListedJobs
    logDebug $ "Blacklisted job IDs: " <> show (map (\(AnyJob Job {..}) -> id.getId) blacklistedTasks)
  tasksPerIteration <- asks (.tasksPerIteration)
  availableReadyTasksIds <- pickTasks tasksPerIteration $ map (\(AnyJob Job {..}) -> parentJobId) nonBlacklistedTasks
  logTagDebug "Available tasks - Count" . show $ length availableReadyTasksIds
  logTagDebug "Available tasks" . show $ availableReadyTasksIds
  takenTasksUpdatedInfo <- getTasksById availableReadyTasksIds
  mapConcurrently runTask takenTasksUpdatedInfo
  where
    isBlacklistedDB :: [Text] -> AnyJob t -> Bool
    isBlacklistedDB blackList (AnyJob Job {..}) =
      let jobType' = show (fromSing $ jobType jobInfo)
       in jobType' `elem` blackList

    pickTasks :: Int -> [Id AnyJob] -> SchedulerM [Id AnyJob]
    pickTasks _ [] = pure []
    pickTasks 0 _ = pure []
    pickTasks tasksRemain (x : xs) = do
      gainedLock <- attemptTaskLockAtomic x
      if gainedLock
        then (x :) <$> pickTasks (tasksRemain - 1) xs
        else pickTasks tasksRemain xs

registerDuration :: Text -> Milliseconds -> a -> SchedulerM ()
registerDuration jobType millis _ = addGenericLatency ("Job_execution_" <> jobType) millis

-- TODO: refactor the prometheus metrics to measure data that we really need

attemptTaskLockAtomic :: Id AnyJob -> SchedulerM Bool
attemptTaskLockAtomic jobId = do
  expirationTime <- asks (.expirationTime)
  Hedis.tryLockRedis jobId.getId (fromInteger expirationTime)

-- TODO: refactor this function so that there was no duplication with the `tryLockRedis` function

releaseLock :: Id AnyJob -> SchedulerM ()
releaseLock jobId = Hedis.unlockRedis jobId.getId

-- TODO: think about more robust style of working with redis locks
-- see https://redis.io/docs/reference/patterns/distributed-locks/

executeTask :: forall t. (JobProcessor t) => SchedulerHandle t -> AnyJob t -> SchedulerM ExecutionResult
executeTask SchedulerHandle {..} (AnyJob job) = do
  schedulerType <- asks (.schedulerType)
  let jobType' = show (fromSing $ jobType $ jobInfo job)
  let begTime = job.scheduledAt
  endTime <- getCurrentTime
  let diff = T.diffUTCTime endTime begTime
      diffInMill = Milliseconds (div (fromEnum diff) 1000000000)
  logDebug $ "diffTime in picking up the job : " <> show diffInMill
  fork "" $ addGenericLatency ("Job_pickup_" <> jobType') $ diffInMill
  case findJobHandlerFunc job jobHandlers of
    Nothing -> failExecution jobType' "No handler function found for the job type = "
    Just handlerFunc_ -> do
      -- TODO: Fix this logic, that's not how we have to handle this issue
      latestState' <- case schedulerType of
        RedisBased -> pure [AnyJob job]
        DbBased -> getTasksById [id job]
      case latestState' of
        [AnyJob latestState] ->
          if scheduledAt latestState > scheduledAt job || status latestState /= Pending
            then pure DuplicateExecution
            else do
              handlerFunc_ job
        _ -> failExecution jobType' "Found multiple tasks by single id."
  where
    failExecution jobType' description = do
      logError $ "failed to execute job: " <> description <> jobType'
      -- logPretty ERROR "failed job" job
      markAsFailed jobType' job.id
      pure $ Terminate description

registerExecutionResult :: forall t. (JobProcessor t) => SchedulerHandle t -> AnyJob t -> ExecutionResult -> SchedulerM ()
registerExecutionResult SchedulerHandle {..} j@(AnyJob job@Job {..}) result = do
  let jobType' = show (fromSing $ jobType jobInfo)
      storedJobInfo = storeJobInfo jobInfo
  when (storedJobType storedJobInfo == "SendSearchRequestToDriver") $
    logDebug $ "Executed scheduled search entry: " <> show (toJSON (AnyJob job))
  logDebug $ "Current Job Id with Status : " <> show id <> " " <> show result
  case result of
    DuplicateExecution -> do
      logInfo $ "job id " <> show id <> " already executed "
    Complete -> do
      logInfo $ "job successfully completed on try " <> show (currErrors + 1)
      markAsComplete jobType' job.id
      fork "" $ incrementStreamCounter ("Executor_" <> show jobType')
    Terminate description -> do
      logError $ "job terminated on try " <> show (currErrors + 1) <> "; with jobId: " <> show job.id <> "; reason: " <> description
      markAsFailed jobType' job.id
      fork "" $ incrementStreamFailedCounter ("Executor_" <> show jobType')
    ReSchedule reScheduledTime -> do
      logInfo $ "job rescheduled on time = " <> show reScheduledTime <> " jobType :" <> jobType'
      reSchedule jobType' j reScheduledTime
    Retry -> do
      isLong <- isJobLongRunning jobType'
      if not isLong
        then -- Non-longRunning retries are requeued onto the stream (Part B) by handleStreamRetry,
        -- after the locks are released. Skip the DB/sorted-set reschedule here to avoid double-enqueue.
          logDebug $ "retry deferred to stream requeue for non-longRunning jobId:" <> show job.id
        else
          let newErrorsCount = job.currErrors + 1
           in if newErrorsCount >= job.maxErrors
                then do
                  logError $ "retries amount exceeded for jobId:" <> show job.id <> ", job failed after try " <> show newErrorsCount
                  updateErrorCountAndFail jobType' job.id newErrorsCount
                else do
                  logError $ "try " <> show newErrorsCount <> " was not successful for jobId:" <> show job.id <> ", trying again"
                  waitBeforeRetry <- asks (.waitBeforeRetry)
                  now <- getCurrentTime
                  reScheduleOnError jobType' j newErrorsCount $
                    fromIntegral waitBeforeRetry `addUTCTime` now

defaultCatcher :: Text -> SomeException -> SchedulerM ExecutionResult
defaultCatcher jobType' exep = do
  jobRetryOnExceptionMap <- asks (.jobRetryOnExceptionMap)
  let isRetryable = fromMaybe False (M.lookup jobType' jobRetryOnExceptionMap)
  if isRetryable
    then do
      logError $ "job execution threw an uncaught exception, will retry within maxErrors budget: jobType = " <> jobType' <> "; exception = " <> show exep
      pure Retry
    else do
      logError $ "job execution threw an uncaught exception, jobType = " <> jobType' <> " is not configured for retry, terminating: " <> show exep
      pure $ Terminate (show exep)

-- ===========================================================================================
-- Stream reliability: stream-native retry, PEL reclaim, consumer heartbeat/liveness, cleanup.
-- All Redis stream/group/registry ops go through withNonCriticalCrossAppRedis so they target the
-- same stream/group as the existing xAdd/xAck/xDel (the authoritative "where the stream lives").
-- ===========================================================================================

-- | Redis key of the per-(stream,group) liveness registry: a ZSET of consumerName -> lastHeartbeat.
mkRegistryKey :: SchedulerM Text
mkRegistryKey = do
  streamName <- asks (.streamName)
  groupName <- asks (.groupName)
  pure $ "Scheduler:LiveConsumers:" <> streamName <> ":" <> groupName

-- | Cross-pod lock so only one pod sweeps dead consumers at a time.
mkSweepLockKey :: Text -> Text
mkSweepLockKey streamName = "Scheduler:ConsumerSweep:Lock:" <> streamName

-- | Per-job "currently executing" lock key. Same key (and typo) the executor and reviver use, so the
-- reclaimer can tell an in-flight job from a truly-orphaned one.
mkRunningJobKey :: Text -> Text
mkRunningJobKey jobId = "RunnningJob:" <> jobId

-- | Per-job stream-retry counter key (bounds Part B requeues + reclaim re-adds by the same budget).
mkStreamRetryCounterKey :: Id AnyJob -> Text
mkStreamRetryCounterKey jobId = "JobStreamRetryCount:" <> jobId.getId

streamRetryCounterTtlSec :: Int
streamRetryCounterTtlSec = 86400

isJobLongRunning :: Text -> SchedulerM Bool
isJobLongRunning jobType' = do
  jobInfoMap <- asks (.jobInfoMap)
  pure $ fromMaybe False (M.lookup jobType' jobInfoMap)

-- | Wrap an auxiliary loop body so a transient error does not permanently kill the loop thread
-- (loopGracefully's @loop@ does not catch; a throw would stop re-running that element forever).
auxLoopBody :: Text -> SchedulerM () -> SchedulerM ()
auxLoopBody tag act = act `C.catchAll` \e -> logError ("SCHEDULER_AUX_LOOP_ERROR [" <> tag <> "]: " <> show e)

-- | Re-add a raw job payload (the AnyJob JSON) as a fresh stream entry (new auto-generated id).
reAddToStream :: BS.ByteString -> SchedulerM ()
reAddToStream payload = do
  streamName <- asks (.streamName)
  fieldKey <- generateGUIDText
  void $ Hedis.withNonCriticalCrossAppRedis $ Hedis.xAdd streamName "*" [(DT.encodeUtf8 fieldKey, payload)]

-- | Increment the per-job retry counter (set TTL on first bump). Returns the new count.
incrStreamRetryCounter :: Id AnyJob -> SchedulerM Integer
incrStreamRetryCounter jobId = Hedis.withNonCriticalCrossAppRedis $ do
  let key = mkStreamRetryCounterKey jobId
  n <- Hedis.incr key
  when (n == 1) $ Hedis.expire key streamRetryCounterTtlSec
  pure n

delStreamRetryCounter :: Id AnyJob -> SchedulerM ()
delStreamRetryCounter jobId = Hedis.withNonCriticalCrossAppRedis $ Hedis.del (mkStreamRetryCounterKey jobId)

-- | Requeue a job onto the stream if it is within its retry budget, else fail it terminally.
requeueOrFailByBudget :: forall t. (JobProcessor t) => SchedulerHandle t -> AnyJob t -> BS.ByteString -> SchedulerM ()
requeueOrFailByBudget hnd (AnyJob job) payload = do
  let jobType' = show (fromSing $ jobType job.jobInfo)
  n <- incrStreamRetryCounter job.id
  if n <= toInteger job.maxErrors
    then do
      logDebug $ "stream requeue attempt " <> show n <> " for jobId:" <> show job.id
      reAddToStream payload
    else do
      logError $ "STREAM_RETRY_BUDGET_EXCEEDED jobId=" <> job.id.getId <> " attempts=" <> show n <> " jobType=" <> jobType'
      markAsFailed hnd jobType' job.id
      delStreamRetryCounter job.id

-- | Called at the end of runTask (after locks are released) to drive stream-native retry for
-- non-longRunning jobs. longRunning jobs keep the DB reschedule path in registerExecutionResult.
handleStreamRetry :: forall t. (JobProcessor t) => SchedulerHandle t -> AnyJob t -> Text -> ExecutionResult -> SchedulerM ()
handleStreamRetry hnd anyJob@(AnyJob job) jobType' result = do
  isLong <- isJobLongRunning jobType'
  unless isLong $
    case result of
      Complete -> delStreamRetryCounter job.id
      Retry -> requeueOrFailByBudget hnd anyJob (BL.toStrict $ A.encode anyJob)
      _ -> pure ()

-- | Reclaim idle pending (PEL) entries: XCLAIM them, re-add fresh copies (within budget) and ack+del
-- the originals. @mbConsumer@ scopes the scan to one consumer (dead-consumer drain / graceful
-- self-cleanup); Nothing scans the whole group (the periodic reclaimer).
--
-- @forceReclaim@ controls the in-flight guard. When False (periodic reclaimer) a job whose running
-- lock is still held is left untouched — it is genuinely executing, not orphaned — so we never
-- double-run or falsely fail a slow/longRunning job. When True (dead-consumer drain and graceful
-- self-cleanup) the owning consumer is known dead/stopping, so any lock it left is stale and every
-- entry is recovered regardless. Entries left in-flight are NOT acked/deleted so their real executor
-- still owns them.
reclaimIdlePending :: forall t. (JobProcessor t, FromJSON t) => SchedulerHandle t -> Bool -> Maybe Text -> Integer -> SchedulerM ()
reclaimIdlePending hnd forceReclaim mbConsumer minIdleMs = do
  streamName <- asks (.streamName)
  groupName <- asks (.groupName)
  consumerName <- asks (.consumerName)
  batch <- asks (.reclaimBatch)
  pending <-
    Hedis.withNonCriticalCrossAppRedis . Hedis.withMasterRedis $
      Hedis.xPendingDetail streamName groupName "-" "+" batch (DT.encodeUtf8 <$> mbConsumer)
  let eligibleIds = map Hedis.messageId $ filter (\r -> Hedis.millisSinceLastDelivered r >= minIdleMs) pending
  unless (null eligibleIds) $ do
    let opts = Hedis.defaultXClaimOpts {Hedis.xclaimForce = True}
    claimed <- Hedis.withNonCriticalCrossAppRedis $ Hedis.xClaim streamName groupName consumerName minIdleMs opts eligibleIds
    let records = zip (Hedis.extractRecordIds claimed) (map snd (Hedis.extractKeyValuePairs claimed))
    -- Only ack+del the entries we actually recovered (re-added or terminally failed). In-flight
    -- entries (running lock held, non-force) are left so their real executor can ack them by id.
    ackDelIds <-
      fmap concat . forM records $ \(recordId, payload) ->
        case A.eitherDecode (BL.fromStrict $ DT.encodeUtf8 payload) :: Either String (AnyJob t) of
          Left err -> do
            logError $ "RECLAIM_DECODE_FAILED err=" <> show err
            pure [recordId] -- undecodable poison: drop it
          Right anyJob@(AnyJob job) -> do
            let jobType' = show (fromSing $ jobType job.jobInfo)
            shouldReclaim <-
              if forceReclaim
                then pure True
                else do
                  isLong <- isJobLongRunning jobType'
                  let runningKey = mkRunningJobKey $ if isLong then job.id.getId else job.parentJobId.getId
                  gotLock <- Hedis.withCrossAppRedis $ Hedis.tryLockRedis runningKey 2
                  when gotLock $ Hedis.withCrossAppRedis $ Hedis.unlockRedis runningKey
                  pure gotLock -- got the lock => not in-flight => safe to reclaim
            if shouldReclaim
              then requeueOrFailByBudget hnd anyJob (DT.encodeUtf8 payload) >> pure [recordId]
              else pure []
    unless (null ackDelIds) $ do
      void $ Hedis.withNonCriticalCrossAppRedis $ Hedis.xAck streamName groupName ackDelIds
      void $ Hedis.withNonCriticalCrossAppRedis $ Hedis.xDel streamName ackDelIds
      logInfo $ "RECLAIMED_PENDING count=" <> show (length ackDelIds) <> " consumerFilter=" <> show mbConsumer

-- | Per-pod heartbeat: record this consumer's liveness in the registry ZSET.
heartbeatLoop :: SchedulerM ()
heartbeatLoop = do
  auxLoopBody "heartbeat" $ do
    registryKey <- mkRegistryKey
    consumerName <- asks (.consumerName)
    now <- getCurrentTimestamp
    Hedis.withNonCriticalCrossAppRedis $ Hedis.zAdd registryKey [(now, consumerName)]
  heartbeatIntervalSec <- asks (.heartbeatIntervalSec)
  threadDelaySec heartbeatIntervalSec

-- | Per-pod PEL reclaimer.
reclaimerLoop :: forall t. (JobProcessor t, FromJSON t) => SchedulerHandle t -> SchedulerM ()
reclaimerLoop hnd = do
  minIdleMs <- asks (.reclaimMinIdleMs)
  auxLoopBody "reclaimer" $ reclaimIdlePending hnd False Nothing minIdleMs
  reclaimIntervalSec <- asks (.reclaimIntervalSec)
  threadDelaySec reclaimIntervalSec

-- | Dead-consumer sweeper: prune registry members not seen within deadConsumerThresholdSec, draining
-- their pending first, then XGROUP DELCONSUMER. Runs under a cross-pod lock (one pod at a time).
sweeperLoop :: forall t. (JobProcessor t, FromJSON t) => SchedulerHandle t -> SchedulerM ()
sweeperLoop hnd = do
  auxLoopBody "sweeper" $ do
    streamName <- asks (.streamName)
    Hedis.withNonCriticalCrossAppRedis $
      Hedis.whenWithLockRedis (mkSweepLockKey streamName) 60 $ do
        groupName <- asks (.groupName)
        consumerName <- asks (.consumerName)
        registryKey <- mkRegistryKey
        batch <- asks (.reclaimBatch)
        threshold <- asks (.deadConsumerThresholdSec)
        now <- getCurrentTimestamp
        staleRaw <- Hedis.zRangeByScoreByCount registryKey 0 (now - fromIntegral threshold) 0 batch
        let stale = filter (/= consumerName) [c | raw <- staleRaw, Just (c :: Text) <- [A.decode (BL.fromStrict raw)]]
        forM_ stale $ \c -> do
          reclaimIdlePending hnd True (Just c) 0
          delRes <- RQ.xGroupDelConsumer streamName groupName c
          Hedis.zRem' registryKey [c]
          logInfo $ "PRUNED_DEAD_CONSUMER consumer=" <> c <> " ownedPending=" <> show delRes
  heartbeatIntervalSec <- asks (.heartbeatIntervalSec)
  threadDelaySec heartbeatIntervalSec

-- | Best-effort cleanup on graceful shutdown: drain this pod's own pending back onto the stream,
-- then remove its consumer from the group and the liveness registry. Crash-without-SIGTERM is
-- covered by the reclaimer + sweeper on the surviving pods.
gracefulConsumerCleanup :: forall t. (JobProcessor t, FromJSON t) => SchedulerHandle t -> SchedulerM ()
gracefulConsumerCleanup hnd = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    DbBased -> pure ()
    RedisBased -> do
      streamName <- asks (.streamName)
      groupName <- asks (.groupName)
      consumerName <- asks (.consumerName)
      registryKey <- mkRegistryKey
      reclaimIdlePending hnd True (Just consumerName) 0
      delRes <- RQ.xGroupDelConsumer streamName groupName consumerName
      Hedis.zRem' registryKey [consumerName]
      logInfo $ "GRACEFUL_CONSUMER_CLEANUP consumer=" <> consumerName <> " ownedPending=" <> show delRes
