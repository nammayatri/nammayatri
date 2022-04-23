{-# LANGUAGE DerivingVia #-}

module App.SchedulerExample where

-- FIXME: This entire module is just for example
-- TODO: move it to the integration tests when real usage of the scheduler library appears.

import App.Types (Flow)
import Beckn.Mock.App (MockM, runMock)
import Beckn.Prelude
import Beckn.Scheduler
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall, readDhallConfigDefault)
import Beckn.Utils.GenericPretty (PrettyShow, Showable (..))
import System.Random

runTransporterScheduler :: (SchedulerConfig JobType -> SchedulerConfig JobType) -> IO ()
runTransporterScheduler configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-transport-scheduler"
  runScheduler appCfg runMock schedulerHandlersList

schedulerHandlersList :: JobHandlerList SchedulerT JobType
schedulerHandlersList =
  [ (PrintBananasCount, JobHandler bananasCounterHandler emptyCatchers),
    (PrintCurrentTimeWithErrorProbability, JobHandler timePrinterHandler emptyCatchers),
    (IncorrectDataJobType, JobHandler incorrectDataJobHandler emptyCatchers)
  ]

-----------------
type SchedulerT = MockM SchedulerResources

makeTestJobEntry :: JobType -> d -> JobEntry JobType d
makeTestJobEntry jType jData =
  JobEntry
    { jobType = jType,
      jobData = jData,
      maxErrors = 5
    }

data JobType
  = PrintBananasCount
  | PrintCurrentTimeWithErrorProbability
  | IncorrectDataJobType
  | FakeJobType
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, FromDhall)
  deriving (JobTypeSerializable) via JSONable JobType
  deriving (PrettyShow) via Showable JobType

-----------------
data BananasCount = BananasCount
  { count :: Int,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)
  deriving (JobDataSerializable) via JSONable BananasCount

createBananasCountingJob :: NominalDiffTime -> Flow (Id (Job JobType BananasCount))
createBananasCountingJob scheduleIn = do
  now <- getCurrentTime
  bCount <- liftIO $ randomRIO (1, 10 :: Int)
  createJobIn scheduleIn $ makeTestJobEntry PrintBananasCount $ makeJobData now bCount
  where
    makeJobData now_ bCount_ =
      BananasCount
        { createdAt = now_,
          count = bCount_
        }

bananasCounterHandler :: Job JobType BananasCount -> SchedulerT ExecutionResult
bananasCounterHandler job = do
  logInfo "job of type 1 is being executed: printing job data"
  logPretty INFO "job data" job.jobData
  pure Completed

-----------------
createTimePrinterJob :: NominalDiffTime -> Flow (Id (Job JobType ()))
createTimePrinterJob = flip createJobIn $ makeTestJobEntry PrintCurrentTimeWithErrorProbability ()

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

-----------------
createFakeJob :: NominalDiffTime -> Flow (Id (Job JobType ()))
createFakeJob = flip createJobIn $ makeTestJobEntry FakeJobType ()

-----------------
data IncorrectlySerializable = IncSer
  { foo :: Int,
    bar :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, PrettyShow)
  deriving (FromJSON) via JSONfail IncorrectlySerializable
  deriving (JobDataSerializable) via JSONable IncorrectlySerializable

newtype JSONfail a = JSONfail a

instance FromJSON (JSONfail a) where
  parseJSON _ = fail "fake fail"

createIncorrectDataJob :: NominalDiffTime -> Flow (Id (Job JobType IncorrectlySerializable))
createIncorrectDataJob = flip createJobIn $ makeTestJobEntry IncorrectDataJobType val
  where
    val = IncSer 2 "quux"

incorrectDataJobHandler :: Job JobType IncorrectlySerializable -> SchedulerT ExecutionResult
incorrectDataJobHandler _ = do
  logError "you shouldn't get here"
  pure Completed
