module App where

import API
import Beckn.Exit
import Beckn.Storage.Redis.Config
import qualified Beckn.Tools.Metrics.Init as Metrics
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.FlowLogging as L
import qualified Beckn.Utils.Servant.Server as Server
import Beckn.Utils.Servant.SignatureAuth
import Control.Concurrent
import qualified Data.Map as Map
import Environment
import EulerHS.Prelude hiding (exitSuccess)
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
import Servant
import qualified Service.Runner as Allocator
import qualified Storage.CachedQueries.Organization as Storage

runAllocator :: (AppCfg -> AppCfg) -> IO ()
runAllocator configModifier = do
  config <- configModifier <$> readDhallConfigDefault "allocation-service"
  Metrics.serve config.metricsPort
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname config.loggerConfig
  appEnv <- buildAppEnv config

  R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv do
      _ <-
        try (prepareRedisConnections config.redisCfg)
          >>= handleLeft @SomeException exitConnCheckFailure "Connections check failed. Exception thrown: "
      logInfo "Setting up for signature auth..."
      allProviders <-
        try Storage.loadAllProviders
          >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
      let allShortIds = map ((.shortId.getShortId) &&& (.uniqueKeyId)) allProviders
      let shardMap = config.shards
      logInfo $ "Shard config: " <> show shardMap <> " | Shard count: " <> show (Map.size shardMap)
      modFlowRtWithAuthManagers flowRt appEnv allShortIds

    let settings =
          defaultSettings
            & setGracefulShutdownTimeout (Just $ getSeconds config.graceTerminationPeriod)
            & setInstallShutdownHandler
              (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
            & setPort config.healthcheckPort
    void . forkIO . runSettings settings $
      Server.run healthCheckAPI healthCheck EmptyContext (App.EnvR flowRt' appEnv)

    runFlowR flowRt' appEnv Allocator.run
