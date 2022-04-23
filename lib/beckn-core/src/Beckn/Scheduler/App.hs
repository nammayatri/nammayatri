{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Beckn.Scheduler.App
  ( runScheduler,
    createJobByTime,
    createJobIn,
    emptyCatchers,
  )
where

import Beckn.Exit (exitDBMigrationFailure)
import Beckn.Mock.Utils (threadDelaySec)
import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.JobHandler
import Beckn.Scheduler.Serialization
import Beckn.Scheduler.Storage.Queries
import qualified Beckn.Scheduler.Storage.Queries as Q
import Beckn.Scheduler.Types
import Beckn.Storage.Esqueleto
import Beckn.Storage.Esqueleto.Config (prepareEsqDBEnv)
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import qualified Beckn.Storage.Esqueleto.Queries as Esq
import qualified Beckn.Storage.Esqueleto.Transactionable as Esq
import Beckn.Storage.Hedis (connectHedis)
import qualified Beckn.Storage.Hedis.Queries as Hedis
import Beckn.Types.Common
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.App (getPodName, handleLeft)
import Beckn.Utils.Common
import Beckn.Utils.IOLogging (prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import qualified Data.Map as Map
import UnliftIO.Concurrent (forkIO)

runScheduler ::
  forall t m.
  JobTypeSerializable t =>
  C.MonadThrow m =>
  SchedulerConfig t ->
  (forall q. SchedulerResources -> m q -> IO q) ->
  JobHandlerList m t ->
  IO ()
runScheduler SchedulerConfig {..} runMonad handlersList = do
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  hedisEnv <- connectHedis hedisCfg (\k -> hedisPrefix <> ":" <> k)
  let schedulerResources = SchedulerResources {..}
      transformFunc :: forall q. m q -> IO q
      transformFunc = runMonad schedulerResources
      handlersMap = Map.fromList $ map (second $ transformJobHandler transformFunc) handlersList

  let schedulerEnv = SchedulerEnv {..}
  let runMigrations :: SchedulerM t ()
      runMigrations = do
        eithRes <- migrateIfNeeded migrationPath autoMigrate esqDBCfg
        handleLeft exitDBMigrationFailure "Couldn't migrate database: " eithRes

  runSchedulerM schedulerEnv $ do
    runMigrations
    runner

runner :: (JobTypeSerializable t) => SchedulerM t ()
runner = do
  before <- getCurrentTime
  let errorLogger e = logError $ "error occured: " <> show e
  runnerIteration `C.catchAll` errorLogger
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)
  runner

runnerIteration :: (JobTypeSerializable t) => SchedulerM t ()
runnerIteration = do
  jobType <- asks (.jobType)
  readyTasks <- getReadyTasks jobType
  availableReadyTasksIds <- filterM attemptTaskLockAtomic $ map (.id) readyTasks
  takenTasksUpdatedInfo <- getTasksById availableReadyTasksIds
  let withLogTag' job = withLogTag ("JobId=" <> job.id.getId)
  forM_ takenTasksUpdatedInfo $ \job ->
    forkIO $
      withLogTag' job $ do
        executeTask job
        releaseLock job.id

attemptTaskLock :: Id (Job a b) -> SchedulerM t Bool
attemptTaskLock jobId = do
  expirationTime <- asks (.expirationTime)
  successfulSet <- Hedis.setNx jobId.getId ()
  if successfulSet
    then do
      Hedis.expire jobId.getId expirationTime
      pure True
    else pure False

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

withJobDataDecoded :: forall d t m. (JobDataSerializable d, Log m, Monad m) => Job t Text -> (Job t d -> m ExecutionResult) -> m ExecutionResult
withJobDataDecoded txtDataJob action =
  maybe errHandler successHandler $ jobDataFromText @d txtDataJob.jobData
  where
    errHandler = do
      logError $ "failed to decode job data: " <> txtDataJob.jobData
      pure Terminate
    successHandler jobData_ = action $ setJobData jobData_ txtDataJob

executeTask :: forall t. (JobTypeSerializable t) => JobText -> SchedulerM t ()
executeTask rawJob = do
  let eithTypeDecodedJob = decodeJob @t @Text rawJob
  case eithTypeDecodedJob of
    Left err -> failJob rawJob $ "type decode failure: " <> show err
    Right decJob -> do
      hMap <- asks (.handlersMap)
      let decJobType = decJob.jobType
      case Map.lookup decJobType hMap of
        Nothing -> failJob rawJob $ "no handler function for the job type = " <> show decJobType
        Just jH -> executeTypeDecodedJob jH decJob
  where
    executeTypeDecodedJob :: JobHandler IO t -> Job t Text -> SchedulerM t ()
    executeTypeDecodedJob (JobHandler handlerFunc_ errorCatchers_) job = do
      result <- withJobDataDecoded job $ \decJob -> do
        let totalErrorCatchers = errorCatchers_ decJob ++ [resultCatcher, defaultCatcher]
        liftIO $ handlerFunc_ decJob `C.catches` totalErrorCatchers
      case result of
        Completed -> do
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

resultCatcher :: C.Handler IO ExecutionResult
resultCatcher = C.Handler pure

defaultCatcher :: C.Handler IO ExecutionResult
defaultCatcher = C.Handler $ const @_ @SomeException $ pure Retry

emptyCatchers :: Job t d -> [C.Handler m b]
emptyCatchers = const []

-- api

type SchedulingConstraints m r t d =
  ( HasEsqEnv r m,
    MonadGuid m,
    MonadCatch m,
    Log m,
    JobTypeSerializable t,
    JobDataSerializable d,
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
    JobTypeSerializable a,
    JobDataSerializable b,
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
          status = PENDING
        }
