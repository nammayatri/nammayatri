module Beckn.Scheduler.App where

import Beckn.Mock.Utils (threadDelaySec)
import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.Storage.Queries
import qualified Beckn.Scheduler.Storage.Queries as Q
import Beckn.Scheduler.Types
import Beckn.Storage.Esqueleto
import Beckn.Storage.Esqueleto.Config (prepareEsqDBEnv)
import qualified Beckn.Storage.Esqueleto.Transactionable as Esq
import Beckn.Storage.Hedis (connectHedis)
import qualified Beckn.Storage.Hedis.Queries as Hedis
import Beckn.Types.Common (MonadGuid (generateGUIDText), MonadTime (getCurrentTime))
import Beckn.Types.Id
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Common (addUTCTime, diffUTCTime)
import Beckn.Utils.IOLogging (prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import UnliftIO.Concurrent (forkIO)

runScheduler ::
  forall m.
  C.MonadThrow m =>
  SchedulerConfig ->
  (forall a. SchedulerResources -> m a -> IO a) ->
  (Job -> [C.Handler m ExecutionResult]) ->
  (Job -> m ExecutionResult) ->
  IO ()
runScheduler SchedulerConfig {..} runMonad errorCatchersM handlerFuncM = do
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  hedisEnv <- connectHedis hedisCfg (\k -> hedisPrefix <> ":" <> k)
  let schedulerResources = SchedulerResources {..}
      transformFunc :: forall b. m b -> IO b
      transformFunc = runMonad schedulerResources
      handlerFunc = transformFunc . handlerFuncM
      errorCatchers = map (transformHandler transformFunc) . errorCatchersM
  let schedulerEnv = SchedulerEnv {..}
  runSchedulerM schedulerEnv runner

transformHandler :: (forall a. m a -> n a) -> C.Handler m b -> C.Handler n b
transformHandler trans (C.Handler actionM) = C.Handler $ trans . actionM

runner :: SchedulerM ()
runner = do
  before <- getCurrentTime
  runnerIteration
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)
  runner

runnerIteration :: SchedulerM ()
runnerIteration = do
  readyTasks <- getReadyTasks
  availableReadyTasks <- filterM attemptTaskLock readyTasks
  takenTasksUpdatedInfo <- getTasksById $ map (.id) availableReadyTasks
  mapM_ (forkIO . executeTask) takenTasksUpdatedInfo

attemptTaskLock :: Job -> SchedulerM Bool
attemptTaskLock job = do
  expirationTime <- asks (.expirationTime)
  successfulSet <- Hedis.setNx job.id.getId ()
  if successfulSet
    then do
      Hedis.expire job.id.getId expirationTime
      pure True
    else pure False

executeTask :: Job -> SchedulerM ()
executeTask job = do
  handlerFn <- asks (.handlerFunc)
  catchers <- asks (.errorCatchers)
  result <- liftIO $ handlerFn job `C.catches` (catchers job ++ [resultCatcher, defaultCatcher])
  case result of
    Completed -> markAsComplete job.id
    Terminate -> markAsTerminated job.id
    ReSchedule reScheduledTime -> reSchedule job.id reScheduledTime
    Retry ->
      let newErrorsCount = job.currErrors + 1
       in if newErrorsCount >= job.maxErrors
            then updateErrorCountAndTerminate job.id newErrorsCount
            else do
              updateFailureCount job.id newErrorsCount
              waitBeforeRetry <- asks (.waitBeforeRetry)
              threadDelaySec waitBeforeRetry
              executeTask job {currErrors = newErrorsCount}

resultCatcher :: C.Handler IO ExecutionResult
resultCatcher = C.Handler pure

defaultCatcher :: C.Handler IO ExecutionResult
defaultCatcher = C.Handler $ const @_ @SomeException $ pure Retry

-- api

createJobIn :: (HasEsqEnv r m, MonadGuid m) => NominalDiffTime -> JobEntry -> m (Id Job)
createJobIn diff jobEntry = do
  now <- getCurrentTime
  let scheduledAt = addUTCTime diff now
  createJob scheduledAt jobEntry

createJob :: (HasEsqEnv r m, MonadGuid m) => UTCTime -> JobEntry -> m (Id Job)
createJob scheduledAt jobEntry = do
  now <- getCurrentTime
  id <- Id <$> generateGUIDText
  Esq.runTransaction $ Q.create $ job id now
  pure id
  where
    job id currentTime =
      Job
        { id = id,
          jobType = jobEntry.jobType,
          jobData = jobEntry.jobData,
          scheduledAt = scheduledAt,
          maximumDelay = jobEntry.maximumDelay,
          maxErrors = jobEntry.maxErrors,
          createdAt = currentTime,
          updatedAt = currentTime,
          currErrors = 0,
          status = PENDING
        }
