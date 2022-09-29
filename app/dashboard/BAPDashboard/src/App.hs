{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API
import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import qualified Data.Map.Strict as Map
import Environment
import qualified EulerHS.Runtime as R
import Servant (Context (..))
import qualified Tools.Auth as Auth

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "bap-dashboard" <&> configModifier
  appEnv <- buildAppEnv authTokenCacheKeyPrefix appCfg
  -- Metrics.serve (appCfg.metricsPort) --  do we need it?
  runServerWithHealthCheck appEnv (Proxy @API) handler identity identity context releaseAppEnv \flowRt -> do
    try (prepareRedisConnections $ appCfg.redisCfg)
      >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
    migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    let flowRt' = flowRt {R._httpClientManagers = Map.singleton "default" (R._defaultHttpClientManager flowRt)}
    pure flowRt'
  where
    context =
      Auth.verifyTokenAction @(FlowR AppEnv)
        :. EmptyContext

    authTokenCacheKeyPrefix :: Text
    authTokenCacheKeyPrefix = "BAP-dashboard:authTokenCacheKey:"
