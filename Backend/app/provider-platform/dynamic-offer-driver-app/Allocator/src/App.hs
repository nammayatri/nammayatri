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
import SharedLogic.Allocator.Jobs.Mandate.Execution (startMandateExecutionForDriver)
import SharedLogic.Allocator.Jobs.Mandate.Notification (sendPDNNotificationToDriver)
import SharedLogic.Allocator.Jobs.Mandate.OrderAndNotificationStatusUpdate (notificationAndOrderStatusUpdate)
import SharedLogic.Allocator.Jobs.Overlay.SendOverlay (sendOverlayToDriver)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers)
import SharedLogic.Allocator.Jobs.UnblockDriverUpdate.UnblockDriver
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as Storage

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
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendPaymentReminderToDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . unsubscribeDriverForPaymentOverdue)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . unblockDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . calculateDriverFeeForDrivers)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendPDNNotificationToDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . startMandateExecutionForDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . notificationAndOrderStatusUpdate)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendOverlayToDriver)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . badDebtCalculation)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . retryDocumentVerificationJob)
    }

runDriverOfferAllocator ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runDriverOfferAllocator configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "driver-offer-allocator"
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
    runSchedulerService handlerCfg.schedulerConfig handlerEnv.jobInfoMap handlerEnv.kvConfigUpdateFrequency $ allocatorHandle flowRt' handlerEnv
