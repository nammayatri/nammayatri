{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import qualified Client.Main as CM
import qualified Data.Bool as B
import Environment (HandlerCfg, HandlerEnv, buildHandlerEnv)
import "rider-app" Environment (AppCfg (..), buildAppEnv)
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.External.AadhaarVerification.Gridline.Config
import Kernel.External.Tokenize
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration
import Kernel.Storage.Queries.SystemConfigs as QSC
import Kernel.Types.Error
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App (getPodName, handleLeft, handleLeftIO)
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth (addAuthManagersToFlowRt, prepareAuthManagers)
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import SharedLogic.JobScheduler
import "rider-app" SharedLogic.Scheduler.Jobs.CallPoliceApi
import SharedLogic.Scheduler.Jobs.Chakras
import "rider-app" SharedLogic.Scheduler.Jobs.CheckExotelCallStatusAndNotifyBPP
import "rider-app" SharedLogic.Scheduler.Jobs.CheckMultimodalConfirmFail
import "rider-app" SharedLogic.Scheduler.Jobs.CheckPNAndSendSMS
import "rider-app" SharedLogic.Scheduler.Jobs.CheckRefundStatus
import "rider-app" SharedLogic.Scheduler.Jobs.CrisRecon
import "rider-app" SharedLogic.Scheduler.Jobs.ExecutePaymentIntent
import "rider-app" SharedLogic.Scheduler.Jobs.MetroBusinessHour
import "rider-app" SharedLogic.Scheduler.Jobs.NyRegularInstance
import "rider-app" SharedLogic.Scheduler.Jobs.NyRegularMaster
import "rider-app" SharedLogic.Scheduler.Jobs.PaymentOrderStatusCheck
import "rider-app" SharedLogic.Scheduler.Jobs.Payout.MetroIncentivePayout
import "rider-app" SharedLogic.Scheduler.Jobs.PostRideSafetyNotification
import "rider-app" SharedLogic.Scheduler.Jobs.SafetyCSAlert
import "rider-app" SharedLogic.Scheduler.Jobs.SafetyIVR
import "rider-app" SharedLogic.Scheduler.Jobs.ScheduledRideNotificationsToRider
import "rider-app" SharedLogic.Scheduler.Jobs.ScheduledRidePopupToRider
import "rider-app" SharedLogic.Scheduler.Jobs.UnblockCustomer
import "rider-app" SharedLogic.Scheduler.Jobs.UpdateCrisUtsData
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBecknConfig
import qualified Storage.CachedQueries.Merchant as QMerchant

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
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendTagActionNotification)
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
          & putJobHandlerInList (liftIO . runFlowR flowRt env . updateCrisUtsDataJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . checkMultimodalConfirmFailJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . checkRefundStatusJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . crisReconJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . paymentOrderStatusCheckJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . updateMetroBusinessHour)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runNyRegularMasterJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runNyRegularInstanceJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . unblockCustomer)
    }

runRiderAppScheduler ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runRiderAppScheduler configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "rider-app-scheduler"
  handlerEnv <- buildHandlerEnv handlerCfg
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname handlerCfg.appCfg.loggerConfig
  appEnv <-
    try (buildAppEnv handlerCfg.appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "
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
        allBaps <-
          try QMerchant.loadAllBaps
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allSubscriberIds = map ((.bapId) &&& (.bapUniqueKeyId)) allBaps
        _ <- liftIO $ createCAC handlerCfg.appCfg
        -- Load FRFS BAPs
        frfsBap <-
          try QBecknConfig.findAll
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allFRFSSubIds = map ((.subscriberId) &&& (.uniqueKeyId)) frfsBap
        flowRt' <-
          addAuthManagersToFlowRt
            flowRt
            $ catMaybes
              [ Just (Nothing, prepareAuthManagers flowRt appEnv allSubscriberIds),
                Just (Nothing, prepareAuthManagers flowRt appEnv allFRFSSubIds),
                Just (Just 150000, prepareGridlineHttpManager 150000),
                Just (Just 10000, prepareJourneyMonitoringHttpManager 10000)
              ]
        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt'
    runSchedulerService handlerCfg.schedulerConfig handlerEnv.jobInfoMap handlerEnv.kvConfigUpdateFrequency handlerEnv.maxShards $ schedulerHandle flowRt' handlerEnv

createCAC :: Environment.AppCfg -> IO ()
createCAC appCfg = do
  when appCfg.cacConfig.enableCac $ do
    cacStatus <- CM.initCACClient appCfg.cacConfig.host (fromIntegral appCfg.cacConfig.interval) appCfg.cacTenants appCfg.cacConfig.enablePolling
    case cacStatus of
      0 -> CM.startCACPolling appCfg.cacTenants
      _ -> do
        -- logError "CAC client failed to start"
        threadDelay 1000000
        B.bool (pure ()) (createCAC appCfg) appCfg.cacConfig.retryConnection
  when appCfg.superPositionConfig.enableSuperPosition $ do
    superPositionStatus <- CM.initSuperPositionClient appCfg.superPositionConfig.host (fromIntegral appCfg.superPositionConfig.interval) appCfg.superPositionConfig.tenants appCfg.superPositionConfig.enablePolling
    case superPositionStatus of
      0 -> CM.runSuperPositionPolling appCfg.superPositionConfig.tenants
      _ -> do
        -- logError "CAC super position client failed to start"
        threadDelay 1000000
        B.bool (pure ()) (createCAC appCfg) appCfg.cacConfig.retryConnection
