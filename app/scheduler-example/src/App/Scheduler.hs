{-# LANGUAGE DerivingVia #-}

module App.Scheduler where

-- FIXME: This entire module is just for example
-- TODO: move it to the integration tests when real usage of the scheduler library appears.

import App.Scheduler.Types
import Beckn.Mock.App (MockM, runMock)
import Beckn.Prelude
import Beckn.Randomizer
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.IOLogging (LoggerEnv, prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import Environment (Flow)
import Lib.Scheduler
import Lib.Scheduler.ScheduleJob (createJobIn)
import Storage as QSJ

schedulerHandle :: LoggerResources -> SchedulerHandle SchedulerJobType
schedulerHandle loggerRes =
  SchedulerHandle
    { getTasksById = QSJ.getTasksById,
      getReadyTasks = QSJ.getReadyTasks,
      markAsComplete = QSJ.markAsComplete,
      markAsFailed = QSJ.markAsFailed,
      updateErrorCountAndFail = QSJ.updateErrorCountAndFail,
      reSchedule = QSJ.reSchedule,
      updateFailureCount = QSJ.updateFailureCount,
      reScheduleOnError = QSJ.reScheduleOnError,
      jobHandlers =
        emptyJobHandlerList
          & putJobHandlerInList (liftIO . runMock loggerRes . bananasCounterHandler)
          & putJobHandlerInList (liftIO . runMock loggerRes . timePrinterHandler)
          & putJobHandlerInList (liftIO . runMock loggerRes . incorrectDataJobHandler)
          & putJobHandlerInList (liftIO . runMock loggerRes . testTerminationHandler)
    }

runExampleScheduler :: (SchedulerConfig -> SchedulerConfig) -> IO ()
runExampleScheduler configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "scheduler-example-scheduler"
  let loggerConfig = appCfg.loggerConfig
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  let loggerRes = LoggerResources {..}
  runSchedulerService appCfg $ schedulerHandle loggerRes

-----------------

data LoggerResources = LoggerResources
  { loggerEnv :: LoggerEnv,
    loggerConfig :: LoggerConfig
  }

type SchedulerT = MockM LoggerResources

makeTestJobEntry :: forall e. (JobTypeConstaints e) => JobContent e -> JobEntry e
makeTestJobEntry jData =
  JobEntry
    { jobData = jData,
      maxErrors = 5
    }

-----------------

createBananasCountingJob :: NominalDiffTime -> Flow (Id (AnyJob SchedulerJobType))
createBananasCountingJob scheduleIn = do
  now <- getCurrentTime
  bCount <- getRandomInRange (1, 10 :: Int)
  createJobIn createJobFunc scheduleIn $ makeTestJobEntry @'PrintBananasCount $ makeJobData now bCount
  where
    makeJobData now_ bCount_ =
      BananasCount
        { createdAt = now_,
          count = bCount_
        }

bananasCounterHandler :: Job 'PrintBananasCount -> SchedulerT ExecutionResult
bananasCounterHandler job = do
  logInfo "job of type 1 is being executed: printing job data"
  logPretty INFO "job data" job.jobData
  pure Complete

-----------------

createTimePrinterJob :: NominalDiffTime -> Flow (Id (AnyJob SchedulerJobType))
createTimePrinterJob scheduleIn =
  createJobIn createJobFunc scheduleIn $ makeTestJobEntry @'PrintCurrentTimeWithErrorProbability ()

timePrinterHandler :: Job 'PrintCurrentTimeWithErrorProbability -> SchedulerT ExecutionResult
timePrinterHandler _ = do
  logInfo "job of type 2 is being executed: trying to print current time with some probability of an error"
  randomNum <- getRandomInRange (1 :: Int, 6)
  if randomNum >= 3
    then throwError $ InternalError "Time printing error"
    else do
      now <- getCurrentTime
      logInfo $ "current time: " <> show now
      pure Complete

-----------------

createFakeJob :: NominalDiffTime -> Flow (Id (AnyJob SchedulerJobType))
createFakeJob scheduleIn =
  createJobIn createJobFunc scheduleIn $ makeTestJobEntry @'FakeJobType ()

-----------------

createIncorrectDataJob :: NominalDiffTime -> Flow (Id (AnyJob SchedulerJobType))
createIncorrectDataJob scheduleIn =
  createJobIn createJobFunc scheduleIn $ makeTestJobEntry @'IncorrectDataJobType val
  where
    val = IncSer 2 "quux"

incorrectDataJobHandler :: Job 'IncorrectDataJobType -> SchedulerT ExecutionResult
incorrectDataJobHandler _ = do
  logError "you shouldn't get here"
  pure Complete

-----------------

createTestTerminationJob :: NominalDiffTime -> Flow (Id (AnyJob SchedulerJobType))
createTestTerminationJob scheduleIn =
  createJobIn createJobFunc scheduleIn $ makeTestJobEntry @'TestTermination ()

testTerminationHandler :: Job 'TestTermination -> SchedulerT ExecutionResult
testTerminationHandler _ = flip C.catchAll (\_ -> pure Retry) $ do
  logDebug "before pause"
  threadDelaySec 10
  logDebug "after pause"
  pure Complete

------------------
