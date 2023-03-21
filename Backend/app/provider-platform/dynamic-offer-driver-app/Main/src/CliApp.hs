module CliApp where

import qualified Data.Text as T
import Environment
import qualified EulerHS.Runtime as R
import Kernel.Exit
import Kernel.Prelude
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import SharedLogic.Allocator.Jobs.UpdateRecurringBookingTimetable (updateRecurringBookingTimetable)
import System.Environment

withAppEnv :: AppCfg -> (AppEnv -> IO a) -> IO a
withAppEnv appCfg =
  bracket
    ( try (buildAppEnv appCfg)
        >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "
    )
    releaseAppEnv

main :: [String] -> IO ()
main args = do
  appCfg <- readDhallConfigDefault "dynamic-offer-driver-app"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  withAppEnv appCfg $ \appEnv ->
    R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
      runFlowR flowRt appEnv $ do
        case args of
          ["update-timetables"] -> updateTimetables
          ["create-upcoming-rides"] -> createUpcomingRides
          _ -> unknownCommand

updateTimetables :: FlowR AppEnv ()
updateTimetables = do
  updateRecurringBookingTimetable

createUpcomingRides :: FlowR AppEnv ()
createUpcomingRides = pure ()

unknownCommand :: FlowR AppEnv ()
unknownCommand = pure ()
