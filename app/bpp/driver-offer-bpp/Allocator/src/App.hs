module App where

import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration
import Beckn.Types.Flow (runFlowR)
import Beckn.Utils.App (getPodName, handleLeft)
import Beckn.Utils.Common
import Beckn.Utils.Dhall
import qualified Beckn.Utils.FlowLogging as L
import Beckn.Utils.Servant.SignatureAuth
import Environment (HandlerCfg, HandlerEnv, buildHandlerEnv)
import qualified EulerHS.Runtime as R
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers)
import qualified Storage.CachedQueries.Merchant as Storage
import qualified Storage.Queries.AllocatorJob as QAllJ

allocatorHandle :: R.FlowRuntime -> HandlerEnv -> SchedulerHandle AllocatorJobType
allocatorHandle flowRt env =
  SchedulerHandle
    { getTasksById = QAllJ.getTasksById,
      getReadyTasks = QAllJ.getReadyTasks,
      markAsComplete = QAllJ.markAsComplete,
      markAsFailed = QAllJ.markAsFailed,
      updateErrorCountAndFail = QAllJ.updateErrorCountAndFail,
      reSchedule = QAllJ.reSchedule,
      updateFailureCount = QAllJ.updateFailureCount,
      reScheduleOnError = QAllJ.reScheduleOnError,
      jobHandlers =
        emptyJobHandlerList
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendSearchRequestToDrivers)
    }

runDriverOfferAllocator ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runDriverOfferAllocator configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "driver-offer-allocator"
  handlerEnv <- buildHandlerEnv handlerCfg
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname handlerCfg.appCfg.loggerConfig

  R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt handlerEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded handlerCfg.appCfg.migrationPath handlerCfg.appCfg.autoMigrate handlerCfg.appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "

        logInfo "Setting up for signature auth..."
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allSubscriberIds = map ((.subscriberId.getShortId) &&& (.uniqueKeyId)) allProviders
        flowRt' <- modFlowRtWithAuthManagers flowRt handlerEnv allSubscriberIds

        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt'
    runSchedulerService handlerCfg.schedulerConfig $ allocatorHandle flowRt' handlerEnv
