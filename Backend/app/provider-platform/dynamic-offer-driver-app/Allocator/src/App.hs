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
import "dynamic-offer-driver-app" Environment (AppCfg (..))
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.External.Verification.Interface.Idfy
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration
import Kernel.Storage.Queries.SystemConfigs as QSC
import Kernel.Types.Error
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App (getPodName, handleLeft)
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.Document.VerificationRetry
import SharedLogic.Allocator.Jobs.DriverFeeUpdates.BadDebtCalculationScheduler
import SharedLogic.Allocator.Jobs.DriverFeeUpdates.DriverFee
import SharedLogic.Allocator.Jobs.FCM.RunScheduledFCMS (runScheduledFCMS)
import SharedLogic.Allocator.Jobs.FCM.SoftBlockNotification
import SharedLogic.Allocator.Jobs.FleetAlert.SendFleetAlert (sendFleetAlert)
import SharedLogic.Allocator.Jobs.Mandate.Execution (startMandateExecutionForDriver)
import SharedLogic.Allocator.Jobs.Mandate.Notification (sendPDNNotificationToDriver)
import SharedLogic.Allocator.Jobs.Mandate.OrderAndNotificationStatusUpdate (notificationAndOrderStatusUpdate)
import SharedLogic.Allocator.Jobs.Overlay.SendOverlay (sendOverlayToDriver)
import SharedLogic.Allocator.Jobs.Payout.DriverReferralPayout (sendDriverReferralPayoutJobData)
import SharedLogic.Allocator.Jobs.ScheduledRides.CheckExotelCallStatusAndNotifyBAP (checkExotelCallStatusAndNotifyBAP)
import SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate (sendScheduledRideAssignedOnUpdate)
import SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideNotificationsToDriver (sendScheduledRideNotificationsToDriver)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers)
import SharedLogic.Allocator.Jobs.SupplyDemand.SupplyDemandRatio
import SharedLogic.Allocator.Jobs.UnblockDriverUpdate.UnblockDriver
import SharedLogic.Allocator.Jobs.Webhook.Webhook
import SharedLogic.KaalChakra.Chakras
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as Storage

createCAC :: AppCfg -> IO ()
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

allocatorHandle :: R.FlowRuntime -> HandlerEnv -> SchedulerHandle AllocatorJobType
allocatorHandle flowRt env =
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
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendSearchRequestToDrivers)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . unblockDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . softBlockNotifyDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . unblockSoftBlockedDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . calculateSupplyDemand)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . calculateDriverFeeForDrivers)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendPDNNotificationToDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . startMandateExecutionForDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . notificationAndOrderStatusUpdate)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendOverlayToDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . badDebtCalculation)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendManualPaymentLink)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . retryDocumentVerificationJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendDriverReferralPayoutJobData)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendScheduledRideNotificationsToDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendScheduledRideAssignedOnUpdate)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . checkExotelCallStatusAndNotifyBAP)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendFleetAlert)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runDailyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runWeeklyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runMonthlyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runQuarterlyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runDailyUpdateTagJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runWeeklyUpdateTagJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runMonthlyUpdateTagJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runQuarterlyUpdateTagJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runScheduledFCMS)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendWebhookWithRetryToExternal)
    }

runDriverOfferAllocator ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runDriverOfferAllocator configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "driver-offer-allocator"
  handlerEnv <- buildHandlerEnv handlerCfg
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname handlerCfg.appCfg.loggerConfig
  _ <- liftIO $ createCAC handlerCfg.appCfg
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
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allSubscriberIds = map ((.subscriberId.getShortId) &&& (.uniqueKeyId)) allProviders
        flowRt' <-
          addAuthManagersToFlowRt
            flowRt
            $ catMaybes
              [ Just (Nothing, prepareAuthManagers flowRt handlerEnv allSubscriberIds),
                Just (Just 20000, prepareIdfyHttpManager 20000)
              ]

        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt'
    runSchedulerService handlerCfg.schedulerConfig handlerEnv.jobInfoMap handlerEnv.kvConfigUpdateFrequency handlerEnv.maxShards $ allocatorHandle flowRt' handlerEnv
