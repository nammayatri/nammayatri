{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.Handler
  ( SchedulerHandle (..),
    handler,
  )
where

import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Cont
import Data.Singletons (fromSing)
import qualified Data.Time as T hiding (getCurrentTime)
import Kernel.Prelude hiding (mask, throwIO)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Lib.Scheduler.Environment
import Lib.Scheduler.JobHandler
import Lib.Scheduler.Metrics
import Lib.Scheduler.Types
import UnliftIO

data SchedulerHandle t = SchedulerHandle
  { jobHandlers :: JobHandlersList t,
    getTasksById :: [Id AnyJob] -> SchedulerM [AnyJob t],
    getReadyTasks :: SchedulerM [AnyJob t],
    markAsComplete :: Id AnyJob -> SchedulerM (),
    markAsFailed :: Id AnyJob -> SchedulerM (),
    updateErrorCountAndFail :: Id AnyJob -> Int -> SchedulerM (),
    reSchedule :: Id AnyJob -> UTCTime -> SchedulerM (),
    updateFailureCount :: Id AnyJob -> Int -> SchedulerM (),
    reScheduleOnError :: Id AnyJob -> Int -> UTCTime -> SchedulerM ()
  }

handler :: SchedulerHandle t -> SchedulerM ()
handler hnd = do
  iterSessionId <- generateGUIDText
  before <- getCurrentTime
  withLogTag iterSessionId $ do
    logInfo "Starting runner iteration"
    runnerIteration hnd
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)
  handler hnd

errorLogger :: (Log m, Show a) => a -> m ()
errorLogger e = logError $ "error occured: " <> show e

runnerIteration :: forall t. SchedulerHandle t -> SchedulerM ()
runnerIteration hnd@SchedulerHandle {..} = do
  readyTasks <- getReadyTasks
  logTagDebug "All Tasks - Count" . show $ length readyTasks
  logTagDebug "All Tasks" . show $ map @_ @(Id AnyJob) (\(AnyJob Job {..}) -> id) readyTasks
  tasksPerIteration <- asks (.tasksPerIteration)
  availableReadyTasksIds <- pickTasks tasksPerIteration $ map (\(AnyJob Job {..}) -> id) readyTasks
  logTagDebug "Available tasks - Count" . show $ length availableReadyTasksIds
  logTagDebug "Available tasks" . show $ availableReadyTasksIds
  takenTasksUpdatedInfo <- getTasksById availableReadyTasksIds
  terminationMVar <- newEmptyMVar
  let inspectTermination = modifyMVarMasked_ terminationMVar pure
      waitAll :: MonadUnliftIO m => [Async a] -> m ()
      waitAll = mapConcurrently_ waitCatch
  flip withAsync (waitEitherTerminationOrExecEnd terminationMVar) $
    withAsyncList (map runTask takenTasksUpdatedInfo) $ \asyncList -> do
      res <- race (waitAll asyncList) inspectTermination
      case res of
        Left _ -> pure ()
        Right _ -> do
          mapM_ cancel asyncList
          waitAll asyncList
  where
    waitEitherTerminationOrExecEnd :: MVar () -> Async () -> SchedulerM ()
    waitEitherTerminationOrExecEnd termMVar exec =
      void (waitCatch exec) `C.catchAll` \e -> mask $ \restore -> do
        logInfo "terminating gracefully"
        errorLogger e
        termPeriod <- asks (.graceTerminationPeriod)
        restore (threadDelaySec termPeriod) `C.catchAll` \e' ->
          logInfo "terminating immediately" >> errorLogger e'
        putMVar termMVar ()
        throwIO e

    runTask :: AnyJob t -> SchedulerM ()
    runTask anyJob@(AnyJob Job {..}) = mask $ \restore -> withLogTag ("JobId=" <> id.getId) $ do
      res <- measuringDuration registerDuration $ restore (executeTask hnd anyJob) `C.catchAll` defaultCatcher
      registerExecutionResult hnd anyJob res
      releaseLock id

    pickTasks :: Int -> [Id AnyJob] -> SchedulerM [Id AnyJob]
    pickTasks _ [] = pure []
    pickTasks 0 _ = pure []
    pickTasks tasksRemain (x : xs) = do
      gainedLock <- attemptTaskLockAtomic x
      if gainedLock
        then (x :) <$> pickTasks (tasksRemain - 1) xs
        else pickTasks tasksRemain xs

withAsyncList :: MonadUnliftIO m => [m a] -> ([Async a] -> m b) -> m b
withAsyncList actions func =
  flip runCont func $ traverse (cont . withAsync) actions

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

executeTask :: SchedulerHandle t -> AnyJob t -> SchedulerM ExecutionResult
executeTask SchedulerHandle {..} (AnyJob job) = do
  let begTime = job.scheduledAt
  endTime <- getCurrentTime
  let diff = T.diffUTCTime endTime begTime
  logDebug $ "diffTime in picking up the job : " <> show (nominalDiffTimeToSeconds diff)
  case findJobHandlerFunc job jobHandlers of
    Nothing -> failExecution $ "No handler function found for the job type = " <> show (fromSing $ jobType $ jobInfo job)
    Just handlerFunc_ -> do
      -- TODO: Fix this logic, that's not how we have to handle this issue
      latestState' <- getTasksById [id job]
      case latestState' of
        [AnyJob latestState] ->
          if scheduledAt latestState > scheduledAt job || status latestState /= Pending
            then pure DuplicateExecution
            else do
              handlerFunc_ job
        _ -> failExecution "Found multiple tasks by single it."
  where
    failExecution description = do
      logError $ "failed to execute job: " <> description
      -- logPretty ERROR "failed job" job
      markAsFailed $ id job
      pure $ Terminate description

registerExecutionResult :: SchedulerHandle t -> AnyJob t -> ExecutionResult -> SchedulerM ()
registerExecutionResult SchedulerHandle {..} (AnyJob job@Job {..}) result =
  case result of
    DuplicateExecution -> do
      logInfo $ "job id " <> show id <> " already executed "
    Complete -> do
      logInfo $ "job successfully completed on try " <> show (currErrors + 1)
      markAsComplete id
    Terminate description -> do
      logInfo $ "job terminated on try " <> show (currErrors + 1) <> "; reason: " <> description
      markAsFailed id
    ReSchedule reScheduledTime -> do
      logInfo $ "job rescheduled on time = " <> show reScheduledTime
      reSchedule id reScheduledTime
    Retry ->
      let newErrorsCount = job.currErrors + 1
       in if newErrorsCount >= job.maxErrors
            then do
              logError $ "retries amount exceeded, job failed after try " <> show newErrorsCount
              updateErrorCountAndFail id newErrorsCount
            else do
              logInfo $ "try " <> show newErrorsCount <> " was not successful, trying again"
              waitBeforeRetry <- asks (.waitBeforeRetry)
              now <- getCurrentTime
              reScheduleOnError id newErrorsCount $
                fromIntegral waitBeforeRetry `addUTCTime` now

defaultCatcher :: C.MonadThrow m => SomeException -> m ExecutionResult
defaultCatcher exep = pure $ defaultResult exep

defaultResult :: SomeException -> ExecutionResult
defaultResult exep = Terminate (show exep)
