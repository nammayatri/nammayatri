{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module App.Scheduler where

-- FIXME: This entire module is just for example
-- TODO: move it to the integration tests when real usage of the scheduler library appears.

import App.Scheduler.Types
import qualified Control.Monad.Catch as C
import Environment (Flow)
import Kernel.Mock.App (MockM, runMock)
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.IOLogging (LoggerEnv, prepareLoggerEnv)
import Lib.Scheduler
import Lib.Scheduler.ScheduleJob (createJobIn)
import Storage as QSJ

schedulerHandle :: LoggerResources -> SchedulerHandle SchedulerJobType
schedulerHandle loggerRes =
  SchedulerHandle
    { getTasksById = QSJ.getTasksById,
      getReadyTasks = return [],
      getReadyTask = return [],
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
  runSchedulerService appCfg mempty 0 $ schedulerHandle loggerRes

-----------------

data LoggerResources = LoggerResources
  { loggerEnv :: LoggerEnv,
    loggerConfig :: LoggerConfig
  }

type SchedulerT = MockM LoggerResources

makeTestJobEntry :: forall e. (JobInfoProcessor e) => JobContent e -> JobEntry e
makeTestJobEntry jData =
  JobEntry
    { jobData = jData,
      maxErrors = 5
    }

-----------------

createBananasCountingJob :: NominalDiffTime -> Flow (Id AnyJob)
createBananasCountingJob scheduleIn = do
  now <- getCurrentTime
  uuid <- generateGUIDText
  bCount <- getRandomInRange (1, 10 :: Int)
  createJobIn uuid createJobFunc scheduleIn 1 $ makeTestJobEntry @'PrintBananasCount $ makeJobData now bCount
  where
    makeJobData now_ bCount_ =
      BananasCount
        { createdAt = now_,
          count = bCount_
        }

bananasCounterHandler :: Job 'PrintBananasCount -> SchedulerT ExecutionResult
bananasCounterHandler job = do
  logInfo "job of type 1 is being executed: printing job data"
  logPretty INFO "job data" job.jobInfo.jobData
  pure Complete

-----------------

createTimePrinterJob :: NominalDiffTime -> Flow (Id AnyJob)
createTimePrinterJob scheduleIn = do
  uuid <- generateGUIDText
  createJobIn uuid createJobFunc scheduleIn 1 $ makeTestJobEntry @'PrintCurrentTimeWithErrorProbability ()

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

createFakeJob :: NominalDiffTime -> Flow (Id AnyJob)
createFakeJob scheduleIn =
  createJobIn "supposed to give uuid here" createJobFunc scheduleIn 1 $ makeTestJobEntry @'FakeJobType ()

-----------------

createIncorrectDataJob :: NominalDiffTime -> Flow (Id AnyJob)
createIncorrectDataJob scheduleIn =
  createJobIn "but being lazy did add here" createJobFunc scheduleIn 1 $ makeTestJobEntry @'IncorrectDataJobType val
  where
    val = IncSer 2 "quux"

incorrectDataJobHandler :: Job 'IncorrectDataJobType -> SchedulerT ExecutionResult
incorrectDataJobHandler _ = do
  logError "you shouldn't get here"
  pure Complete

-----------------

createTestTerminationJob :: NominalDiffTime -> Flow (Id AnyJob)
createTestTerminationJob scheduleIn =
  createJobIn "uuid here" createJobFunc scheduleIn 1 $ makeTestJobEntry @'TestTermination ()

testTerminationHandler :: Job 'TestTermination -> SchedulerT ExecutionResult
testTerminationHandler _ = flip C.catchAll (\_ -> pure Retry) $ do
  logDebug "before pause"
  threadDelaySec 10
  logDebug "after pause"
  pure Complete

------------------
