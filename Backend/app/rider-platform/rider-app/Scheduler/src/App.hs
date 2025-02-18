{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import Environment (HandlerCfg, HandlerEnv, buildHandlerEnv)
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration
import Kernel.Storage.Queries.SystemConfigs as QSC
import Kernel.Types.Error
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App (getPodName, handleLeft)
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import SharedLogic.JobScheduler
import "rider-app" SharedLogic.Scheduler.Jobs.CallPoliceApi
import SharedLogic.Scheduler.Jobs.Chakras
import "rider-app" SharedLogic.Scheduler.Jobs.CheckExotelCallStatusAndNotifyBPP
import "rider-app" SharedLogic.Scheduler.Jobs.CheckPNAndSendSMS
import "rider-app" SharedLogic.Scheduler.Jobs.ExecutePaymentIntent
import "rider-app" SharedLogic.Scheduler.Jobs.Payout.MetroIncentivePayout
import "rider-app" SharedLogic.Scheduler.Jobs.PostRideSafetyNotification
import "rider-app" SharedLogic.Scheduler.Jobs.SafetyCSAlert
import "rider-app" SharedLogic.Scheduler.Jobs.SafetyIVR
import "rider-app" SharedLogic.Scheduler.Jobs.ScheduledRideNotificationsToRider
import "rider-app" SharedLogic.Scheduler.Jobs.ScheduledRidePopupToRider
import Storage.Beam.SystemConfigs ()

schedulerHandle :: R.FlowRuntime -> HandlerEnv -> SchedulerHandle RiderJobType
schedulerHandle flowRt env =
  SchedulerHandle
    { getTasksById = QAllJ.getTasksById,
      getReadyTasks = QAllJ.getReadyTasks $ Just env.maxShards,
      getReadyTask = QAllJ.getReadyTask,
      markAsComplete = QAllJ.markAsComplete,
      markAsFailed = QAllJ.markAsFailed,
      updateErrorCountAndFail = QAllJ.updateErrorCountAndFail,
      reSchedule = QAllJ.reSchedule,
      updateFailureCount = QAllJ.updateFailureCount,
      reScheduleOnError = QAllJ.reScheduleOnError,
      jobHandlers =
        emptyJobHandlerList
          & putJobHandlerInList (liftIO . runFlowR flowRt env . checkPNAndSendSMS)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendScheduledRideNotificationsToRider)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendSafetyIVR)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendCallPoliceApi)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . checkExotelCallStatusAndNotifyBPP)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendSafetyCSAlert)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendCustomerRefund)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendScheduledRidePopupToRider)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . executePaymentIntentJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . cancelExecutePaymentIntentJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . postRideSafetyNotification)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runDailyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runWeeklyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runMonthlyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runQuarterlyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runDailyUpdateTagJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runWeeklyUpdateTagJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runMonthlyUpdateTagJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runQuarterlyUpdateTagJob)
    }

runRiderAppScheduler ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runRiderAppScheduler configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "rider-app-scheduler"
  handlerEnv <- buildHandlerEnv handlerCfg
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname handlerCfg.appCfg.loggerConfig

  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow
      flowRt
      ( ( prepareConnectionDriver
            ConnectionConfigDriver
              { esqDBCfg = handlerCfg.appCfg.esqDBCfg,
                esqDBReplicaCfg = handlerCfg.appCfg.esqDBReplicaCfg,
                hedisClusterCfg = handlerCfg.appCfg.hedisClusterCfg
              }
            handlerCfg.appCfg.kvConfigUpdateFrequency
        )
          >> L.setOption KafkaConn handlerEnv.kafkaProducerTools
      )
    -- R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt handlerEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded handlerCfg.appCfg.migrationPath handlerCfg.appCfg.autoMigrate handlerCfg.appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo "Setting up for signature auth..."
        kvConfigs <-
          findById "kv_configs" >>= pure . decodeFromText' @Tables
            >>= fromMaybeM (InternalError "Couldn't find kv_configs table for driver app")
        L.setOption KBT.Tables kvConfigs
        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt
    managers <- managersFromManagersSettings handlerEnv.httpClientOptions.timeoutMs mempty -- default manager is created
    let flowRt'' = flowRt' {R._httpClientManagers = managers}
    runSchedulerService handlerCfg.schedulerConfig handlerEnv.jobInfoMap handlerEnv.kvConfigUpdateFrequency handlerEnv.maxShards $ schedulerHandle flowRt'' handlerEnv
