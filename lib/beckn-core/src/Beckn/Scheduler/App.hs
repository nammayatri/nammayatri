module Beckn.Scheduler.App
  ( runScheduler,
    createJobByTime,
    createJobIn,
  )
where

import Beckn.Exit (exitDBMigrationFailure)
import Beckn.Mock.Utils (threadDelaySec)
import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.JobHandler
import Beckn.Scheduler.Metrics
import Beckn.Scheduler.Storage.Queries
import qualified Beckn.Scheduler.Storage.Queries as Q
import Beckn.Scheduler.Types
import Beckn.Storage.Esqueleto
import Beckn.Storage.Esqueleto.Config (prepareEsqDBEnv)
import Beckn.Storage.Esqueleto.Migration
import qualified Beckn.Storage.Esqueleto.Queries as Esq
import qualified Beckn.Storage.Esqueleto.Transactionable as Esq
import Beckn.Storage.Hedis (connectHedis)
import qualified Beckn.Storage.Hedis.Queries as Hedis
import qualified Beckn.Tools.Metrics.Init as Metrics
import Beckn.Types.Common
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.App (getPodName, handleLeft)
import Beckn.Utils.Common
import Beckn.Utils.IOLogging (prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import qualified Data.Map as Map
import EulerHS.Prelude (exitFailure)
import System.IO (stderr)
import System.Posix (stdError)
import UnliftIO.Concurrent (forkIO)

runScheduler ::
  forall t.
  JobTypeConstraints t =>
  SchedulerConfig t ->
  JobHandlerList t ->
  IO ()
runScheduler SchedulerConfig {..} handlersList = do
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  hedisEnv <- connectHedis hedisCfg (\k -> hedisPrefix <> ":" <> k)
  metrics <- setupSchedulerMetrics
  let handlersMap = Map.fromList handlersList

  let schedulerEnv = SchedulerEnv {..}
  let runMigrations :: SchedulerM t ()
      runMigrations = do
        eithRes <- migrateIfNeededWithinSchema migrationPath autoMigrate esqDBCfg
        handleLeft exitDBMigrationFailure "Couldn't migrate database: " eithRes
  when (tasksPerIteration <= 0) $ do
    hPutStrLn stderr ("tasksPerIteration should be greater than 0" :: Text)
    exitFailure

  Metrics.serve metricsPort
  runSchedulerM schedulerEnv $ do
    runMigrations
    runner

runner :: (JobTypeConstraints t) => SchedulerM t ()
runner = do
  before <- getCurrentTime
  let errorLogger e = logError $ "error occured: " <> show e
  runnerIteration `C.catchAll` errorLogger
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)
  runner

runnerIteration :: (JobTypeConstraints t) => SchedulerM t ()
runnerIteration = do
  jobType <- asks (.jobType)
  readyTasks <- getReadyTasks jobType
  tasksPerIteration <- asks (.tasksPerIteration)
  availableReadyTasksIds <- pickTasks tasksPerIteration $ map (.id) readyTasks
  takenTasksUpdatedInfo <- getTasksById availableReadyTasksIds
  let withLogTag' job = withLogTag ("JobId=" <> job.id.getId)
  forM_ takenTasksUpdatedInfo $ \job ->
    forkIO $
      withLogTag' job $ do
        measuringDuration registerDuration $ executeTask job
        releaseLock job.id
  where
    pickTasks :: Int -> [Id JobText] -> SchedulerM t [Id JobText]
    pickTasks _ [] = pure []
    pickTasks 0 _ = pure []
    pickTasks tasksRemain (x : xs) = do
      gainedLock <- attemptTaskLockAtomic x
      let tasksRemain' = tasksRemain - (if gainedLock then 1 else 0)
      pickTasks tasksRemain' xs

registerDuration :: Milliseconds -> a -> SchedulerM t ()
registerDuration millis _ = do
  let durSecDouble = millisToSecondsDouble millis
  observeJobExecDuration durSecDouble
  logInfo $ "job execution took " <> show (realToFrac @_ @NominalDiffTime durSecDouble)

-- TODO: refactor the prometheus metrics to measure data that we really need

attemptTaskLockAtomic :: Id (Job a b) -> SchedulerM t Bool
attemptTaskLockAtomic jobId = do
  expirationTime <- asks (.expirationTime)
  Hedis.setNxExpire jobId.getId expirationTime ()

releaseLock :: Id (Job a b) -> SchedulerM t ()
releaseLock jobId = Hedis.del jobId.getId

failJob :: JobText -> Text -> SchedulerM t ()
failJob jobText description = do
  logError $ "failed to execute job: " <> description
  logPretty ERROR "failed job" jobText
  markAsTerminated jobText.id

withJobDataDecoded :: forall d t m. (JobDataConstraints d, Log m, Monad m) => Job t Text -> (Job t d -> m ExecutionResult) -> m ExecutionResult
withJobDataDecoded txtDataJob action =
  maybe errHandler successHandler $ decodeFromText @d txtDataJob.jobData
  where
    errHandler = do
      logError $ "failed to decode job data: " <> txtDataJob.jobData
      pure Terminate
    successHandler jobData_ = action $ setJobData jobData_ txtDataJob

executeTask :: forall t. (JobTypeConstraints t) => JobText -> SchedulerM t ()
executeTask rawJob = do
  let eithDecodedType = decodeFromText @t rawJob.jobType
  case eithDecodedType of
    Nothing -> failJob rawJob $ "type decode failure: " <> rawJob.jobType
    Just decJobType -> do
      hMap <- asks (.handlersMap)
      case Map.lookup decJobType hMap of
        Nothing -> failJob rawJob $ "no handler function for the job type = " <> show decJobType
        Just jH -> executeTypeDecodedJob jH $ setJobType decJobType rawJob
  where
    executeTypeDecodedJob :: JobHandler t -> Job t Text -> SchedulerM t ()
    executeTypeDecodedJob (JobHandler handlerFunc_) job = do
      result <- withJobDataDecoded job $ \decJob -> do
        liftIO $ handlerFunc_ decJob `C.catchAll` defaultCatcher
      case result of
        Complete -> do
          logInfo $ "job successfully completed on try " <> show (job.currErrors + 1)
          markAsComplete job.id
        Terminate -> do
          logInfo $ "job terminated on try " <> show (job.currErrors + 1)
          markAsTerminated job.id
        ReSchedule reScheduledTime -> do
          logInfo $ "job rescheduled on time = " <> show reScheduledTime
          reSchedule job.id reScheduledTime
        Retry ->
          let newErrorsCount = job.currErrors + 1
           in if newErrorsCount >= job.maxErrors
                then do
                  logError $ "retries amount exceeded, job failed after try " <> show newErrorsCount
                  updateErrorCountAndTerminate job.id newErrorsCount
                else do
                  logInfo $ "try " <> show newErrorsCount <> " was not successful, trying again"
                  waitBeforeRetry <- asks (.waitBeforeRetry)
                  now <- getCurrentTime
                  reScheduleOnError job.id newErrorsCount $
                    fromIntegral waitBeforeRetry `addUTCTime` now

defaultCatcher :: SomeException -> IO ExecutionResult
defaultCatcher _ = pure Terminate

-- api

type SchedulingConstraints m r t d =
  ( HasEsqEnv r m,
    MonadGuid m,
    MonadCatch m,
    Log m,
    JobTypeConstraints t,
    JobDataConstraints d,
    Show t,
    Show d
  )

createJobIn ::
  SchedulingConstraints m r t d =>
  NominalDiffTime ->
  JobEntry t d ->
  m (Id (Job t d))
createJobIn diff jobEntry = do
  now <- getCurrentTime
  when (diff < 0) $ throwError $ InternalError "job can only be scheduled for now or for future"
  let scheduledAt = addUTCTime diff now
  createJob scheduledAt jobEntry

createJobByTime ::
  SchedulingConstraints m r t d =>
  UTCTime ->
  JobEntry t d ->
  m (Id (Job t d))
createJobByTime scheduledAt jobEntry = do
  now <- getCurrentTime
  when (scheduledAt <= now) $
    throwError $
      InternalError
        "job can only be scheduled for the future\
        \ using createJobByTime, for scheduling for\
        \ now use createJobIn function instead"
  createJob scheduledAt jobEntry

createJob ::
  forall a b r m.
  ( HasEsqEnv r m,
    MonadGuid m,
    MonadCatch m,
    JobTypeConstraints a,
    JobDataConstraints b,
    Show a,
    Show b,
    Log m
  ) =>
  UTCTime ->
  JobEntry a b ->
  m (Id (Job a b))
createJob scheduledAt jobEntry = do
  when (jobEntry.maxErrors <= 0) $ throwError $ InternalError "maximum errors should be positive"
  now <- getCurrentTime
  id <- Id <$> generateGUIDText
  let job = makeJob id now
      jobText = encodeJob job
  Esq.runTransaction $ do
    Q.create jobText
    mbFetchedJob <- Esq.findById jobText.id
    fetchedJob <- fromMaybeM (InternalError "Failed to insert job") mbFetchedJob
    case decodeJob @a @b fetchedJob of
      Left err -> do
        logError $ "failed to decode job:" <> show fetchedJob
        throwError err
      Right decodedJob ->
        unless (typeAndDataAreEqual job decodedJob) $
          logWarning $ "database representations of the inserted and the fetched jobs are not equal: " <> show job <> " : " <> show decodedJob
  pure job.id
  where
    typeAndDataAreEqual job1 job2 = job1.jobData == job2.jobData && job1.jobType == job2.jobType
    makeJob id currentTime =
      Job
        { id = id,
          jobType = jobEntry.jobType,
          jobData = jobEntry.jobData,
          scheduledAt = scheduledAt,
          maxErrors = jobEntry.maxErrors,
          createdAt = currentTime,
          updatedAt = currentTime,
          currErrors = 0,
          status = Pending
        }
