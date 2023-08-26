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
  )
where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad.Catch
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS
import Data.Singletons (fromSing)
import qualified EulerHS.Language as L
import Kernel.Prelude hiding (mask, throwIO)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.Time ()
import Lib.Scheduler.Environment
import Lib.Scheduler.JobHandler
import Lib.Scheduler.Metrics
import Lib.Scheduler.Types

data SchedulerHandle t = SchedulerHandle
  { jobHandlers :: JobHandlersList t,
    getTasksById :: [Id AnyJob] -> SchedulerM [AnyJob t],
    getReadyTasks :: SchedulerM [(AnyJob t, BS.ByteString)],
    markAsComplete :: Text -> Id AnyJob -> SchedulerM (),
    markAsFailed :: Text -> Id AnyJob -> SchedulerM (),
    updateErrorCountAndFail :: Text -> Id AnyJob -> Int -> SchedulerM (),
    reSchedule :: Text -> AnyJob t -> UTCTime -> SchedulerM (),
    updateFailureCount :: Text -> Id AnyJob -> Int -> SchedulerM (),
    reScheduleOnError :: Text -> AnyJob t -> Int -> UTCTime -> SchedulerM ()
  }

handler :: forall t. (JobProcessor t, FromJSON t) => SchedulerHandle t -> SchedulerM ()
handler hnd = do
  maxThreads <- asks (.maxThreads)
  executionChannels :: [Chan (AnyJob t)] <- L.runIO $ mapM (const newChan) [1 .. maxThreads]
  mapM_ (fork "executing tasks" . executeTaskInChan) executionChannels
  handlerLoop hnd executionChannels
  where
    executeTaskInChan :: Chan (AnyJob t) -> SchedulerM ()
    executeTaskInChan ch = runTask =<< L.runIO (readChan ch)

    runTask :: AnyJob t -> SchedulerM ()
    runTask anyJob@(AnyJob Job {..}) = mask $ \restore -> withLogTag ("JobId = " <> id.getId <> " and " <> "parentJobId = " <> parentJobId.getId) $ do
      res <- measuringDuration registerDuration $ restore (executeTask hnd anyJob) `C.catchAll` defaultCatcher
      registerExecutionResult hnd anyJob res
      releaseLock parentJobId

handlerLoop :: (JobProcessor t, FromJSON t) => SchedulerHandle t -> [Chan (AnyJob t)] -> SchedulerM ()
handlerLoop hnd executionChannels = do
  logInfo "Starting runner iteration 1"
  schedulerType <- asks (.schedulerType)
  iterSessionId <- generateGUIDText
  before <- getCurrentTime
  withLogTag iterSessionId $ do
    logInfo "Starting runner iteration"
    case schedulerType of
      RedisBased -> runnerIterationRedis hnd executionChannels
      DbBased -> runnerIteration hnd executionChannels
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)
  handlerLoop hnd executionChannels

mapConcurrently :: Traversable t => (a -> SchedulerM ()) -> t a -> SchedulerM ()
mapConcurrently action = mapM_ (fork "mapThread" . action)

runnerIterationRedis :: forall t. (JobProcessor t, FromJSON t) => SchedulerHandle t -> [Chan (AnyJob t)] -> SchedulerM ()
runnerIterationRedis SchedulerHandle {..} executionChannels = do
  key <- asks (.streamName)
  groupName <- asks (.groupName)
  readyTasks <- getReadyTasks
  logTagDebug "All Tasks - Count" . show $ length readyTasks
  logTagDebug "All Tasks" . show $ map @_ @(Id AnyJob) (\(AnyJob Job {..}, _) -> parentJobId) readyTasks
  filteredTasks <- filterM (\(AnyJob Job {..}, _) -> attemptTaskLockAtomic parentJobId) readyTasks
  let (filteredTasks', recordIds) = foldl (\(ftAcc, rIdAcc) (task, recordId) -> (task : ftAcc, recordId : rIdAcc)) ([], []) filteredTasks
  logTagDebug "Available tasks - Count" . show $ length filteredTasks
  logTagDebug "Available tasks" . show $ map @_ @(Id AnyJob) (\(AnyJob Job {..}, _) -> parentJobId) filteredTasks
  mapConcurrently writeToChan $ zip filteredTasks' $ cycle executionChannels
  unless (null recordIds) do
    void $ Hedis.withNonCriticalCrossAppRedis $ Hedis.xAck key groupName recordIds
    void $ Hedis.withNonCriticalCrossAppRedis $ Hedis.xDel key recordIds
  where
    writeToChan :: (AnyJob t, Chan (AnyJob t)) -> SchedulerM ()
    writeToChan (job, ch) = L.runIO $ writeChan ch job

runnerIteration :: forall t. (JobProcessor t) => SchedulerHandle t -> [Chan (AnyJob t)] -> SchedulerM ()
runnerIteration SchedulerHandle {..} executionChannels = do
  readyJobs <- getReadyTasks
  let readyTasks = map fst readyJobs
  logTagDebug "All Tasks - Count" . show $ length readyTasks
  logTagDebug "All Tasks" . show $ map @_ @(Id AnyJob) (\(AnyJob Job {..}) -> id) readyTasks
  tasksPerIteration <- asks (.tasksPerIteration)
  availableReadyTasksIds <- pickTasks tasksPerIteration $ map (\(AnyJob Job {..}) -> id) readyTasks
  logTagDebug "Available tasks - Count" . show $ length availableReadyTasksIds
  logTagDebug "Available tasks" . show $ availableReadyTasksIds
  takenTasksUpdatedInfo <- getTasksById availableReadyTasksIds
  mapConcurrently writeToChan $ zip takenTasksUpdatedInfo $ cycle executionChannels
  where
    writeToChan :: (AnyJob t, Chan (AnyJob t)) -> SchedulerM ()
    writeToChan (job, ch) = L.runIO $ writeChan ch job

    pickTasks :: Int -> [Id AnyJob] -> SchedulerM [Id AnyJob]
    pickTasks _ [] = pure []
    pickTasks 0 _ = pure []
    pickTasks tasksRemain (x : xs) = do
      gainedLock <- attemptTaskLockAtomic x
      if gainedLock
        then (x :) <$> pickTasks (tasksRemain - 1) xs
        else pickTasks tasksRemain xs

registerDuration :: Milliseconds -> a -> SchedulerM ()
registerDuration millis _ = do
  let durSecDouble = millisToSecondsDouble millis
  observeJobExecDuration durSecDouble
  logInfo $ "job execution took " <> show (realToFrac @_ @NominalDiffTime durSecDouble)

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
  case result of
    DuplicateExecution -> do
      logInfo $ "job id " <> show id <> " already executed "
    Complete -> do
      logInfo $ "job successfully completed on try " <> show (currErrors + 1)
      markAsComplete jobType' job.id
      fork "" $ incrementStreamCounter "Executor"
    Terminate description -> do
      logInfo $ "job terminated on try " <> show (currErrors + 1) <> "; reason: " <> description
      markAsFailed jobType' job.id
    ReSchedule reScheduledTime -> do
      logInfo $ "job rescheduled on time = " <> show reScheduledTime
      reSchedule jobType' j reScheduledTime
    Retry ->
      let newErrorsCount = job.currErrors + 1
       in if newErrorsCount >= job.maxErrors
            then do
              logError $ "retries amount exceeded, job failed after try " <> show newErrorsCount
              updateErrorCountAndFail jobType' job.id newErrorsCount
            else do
              logInfo $ "try " <> show newErrorsCount <> " was not successful, trying again"
              waitBeforeRetry <- asks (.waitBeforeRetry)
              now <- getCurrentTime
              reScheduleOnError jobType' j newErrorsCount $
                fromIntegral waitBeforeRetry `addUTCTime` now

defaultCatcher :: C.MonadThrow m => SomeException -> m ExecutionResult
defaultCatcher exep = pure $ defaultResult exep

defaultResult :: SomeException -> ExecutionResult
defaultResult exep = Terminate (show exep)
