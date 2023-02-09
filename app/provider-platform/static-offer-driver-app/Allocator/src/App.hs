module App where

import API
import Control.Concurrent
import qualified Data.Map as Map
import Environment
import EulerHS.Prelude hiding (exitSuccess)
import qualified EulerHS.Runtime as R
import Kernel.Exit
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import qualified Kernel.Utils.Servant.Server as Server
import Kernel.Utils.Servant.SignatureAuth
import Network.Wai.Handler.Warp
import Servant
import qualified Service.Runner as Allocator
import qualified Storage.CachedQueries.Merchant as Storage

runAllocator :: (AppCfg -> AppCfg) -> IO ()
runAllocator configModifier = do
  config <- configModifier <$> readDhallConfigDefault "allocation-service"
  Metrics.serve config.metricsPort
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname config.loggerConfig
  appEnv <- buildAppEnv config

  R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv do
      logInfo "Setting up for signature auth..."
      allProviders <-
        try Storage.loadAllProviders
          >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
      let allSubscriberIds = map ((.subscriberId.getShortId) &&& (.uniqueKeyId)) allProviders
      let shardMap = config.shards
      logInfo $ "Shard config: " <> show shardMap <> " | Shard count: " <> show (Map.size shardMap)
      modFlowRtWithAuthManagers flowRt appEnv allSubscriberIds

    let settings =
          defaultSettings
            & setGracefulShutdownTimeout (Just $ getSeconds config.graceTerminationPeriod)
            & setInstallShutdownHandler
              (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
            & setPort config.healthcheckPort
    void . forkIO . runSettings settings $
      Server.run healthCheckAPI healthCheck EmptyContext (App.EnvR flowRt' appEnv)

    runFlowR flowRt' appEnv Allocator.run
