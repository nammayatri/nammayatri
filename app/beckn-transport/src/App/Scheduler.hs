module App.Scheduler where

import Beckn.Mock.App (MockM, runMock)
import Beckn.Prelude
import Beckn.Scheduler.App
import Beckn.Scheduler.Environment
import Beckn.Scheduler.Types
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Utils.Common (logInfo, logWarning, throwError)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import System.Random

runTransporterScheduler :: (SchedulerConfig -> SchedulerConfig) -> IO ()
runTransporterScheduler configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-transport-scheduler"
  runScheduler appCfg runMock (const []) handlerFuncTransporter

type SchedulerT = MockM SchedulerResources

handlerFuncTransporter :: Job -> SchedulerT ExecutionResult
handlerFuncTransporter job = case job.jobType of
  "type1" -> logInfo "job of type 1 executed" >> pure Completed
  "type2" -> do
    logInfo "job of type 2 executed"
    randomNum <- liftIO $ randomRIO (1 :: Int, 6)
    if randomNum >= 3
      then throwError $ InternalError "fake error"
      else pure Completed
  _ -> logWarning "unknown job type" >> pure Terminate
