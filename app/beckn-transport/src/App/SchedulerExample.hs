{-# LANGUAGE DerivingVia #-}

module App.SchedulerExample where

-- FIXME: This entire module is just for example
-- Remove it when real usage of the scheduler library appears.

import App.Types (Flow)
import Beckn.Mock.App (MockM, runMock)
import Beckn.Prelude
import Beckn.Scheduler
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.GenericPretty (PrettyShow, Showable (..))
import System.Random

runTransporterScheduler :: (SchedulerConfig -> SchedulerConfig) -> IO ()
runTransporterScheduler configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-transport-scheduler"
  runScheduler appCfg runMock (const []) handlerFuncTransporter

data JobType = PrintBananasCount | PrintCurrentTimeWithErrorProbability
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
  deriving (JobTypeSerializable) via JSONable JobType
  deriving (PrettyShow) via Showable JobType

data BananasCount = BananasCount
  { count :: Int,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)
  deriving (JobDataSerializable) via JSONable BananasCount

withJobDataDecoded :: forall d t m. (JobDataSerializable d, Log m, Monad m) => Job t Text -> (Job t d -> m ExecutionResult) -> m ExecutionResult
withJobDataDecoded txtDataJob action =
  maybe errHandler successHandler $ jobDataFromText @d txtDataJob.jobData
  where
    errHandler = do
      logError $ "failed to decode job data: " <> txtDataJob.jobData
      pure Terminate
    successHandler jobData_ = action $ setJobData jobData_ txtDataJob

type TransporterJob = Job JobType Text

type SchedulerT = MockM SchedulerResources

createTestJob :: (Show a, JobDataSerializable a) => Flow (JobEntry JobType a) -> NominalDiffTime -> Flow (Id (Job JobType a))
createTestJob buildJob scheduleIn = do
  job <- buildJob
  eithRes <- createJobIn scheduleIn job
  fromEitherM identity eithRes

createBananasCountingJob :: NominalDiffTime -> Flow (Id (Job JobType BananasCount))
createBananasCountingJob = createTestJob buildJob
  where
    buildJob = do
      now <- getCurrentTime
      bCount <- liftIO $ randomRIO (1, 10 :: Int)
      pure $
        JobEntry
          { jobType = PrintBananasCount,
            jobData =
              BananasCount
                { createdAt = now,
                  count = bCount
                },
            maxErrors = 5,
            maximumDelay = Nothing
          }

createTimePrinterJob :: NominalDiffTime -> Flow (Id (Job JobType ()))
createTimePrinterJob = createTestJob buildJob
  where
    buildJob = do
      pure $
        JobEntry
          { jobType = PrintCurrentTimeWithErrorProbability,
            jobData = (),
            maxErrors = 5,
            maximumDelay = Nothing
          }

handlerFuncTransporter :: TransporterJob -> SchedulerT ExecutionResult
handlerFuncTransporter job = case job.jobType of
  PrintBananasCount -> withJobDataDecoded job bananasCounterHandler
  PrintCurrentTimeWithErrorProbability -> withJobDataDecoded job timePrinterHandler

bananasCounterHandler :: Job JobType BananasCount -> SchedulerT ExecutionResult
bananasCounterHandler job = do
  logInfo "job of type 1 is being executed: printing job data"
  logPretty INFO "job data" job.jobData
  pure Completed

timePrinterHandler :: Job JobType () -> SchedulerT ExecutionResult
timePrinterHandler _ = do
  logInfo "job of type 2 is being executed: trying to print current time with some probability of an error"
  randomNum <- liftIO $ randomRIO (1 :: Int, 6)
  if randomNum >= 3
    then throwError $ InternalError "Time printing error"
    else do
      now <- getCurrentTime
      logInfo $ "current time: " <> show now
      pure Completed
