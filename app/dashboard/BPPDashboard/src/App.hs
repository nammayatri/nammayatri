{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import Environment
import Servant (Context (..))
import Tools.Auth

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "bpp-dashboard" <&> configModifier
  appEnv <- buildAppEnv appCfg
  -- Metrics.serve (appCfg.metricsPort) --  do we need it?
  runServerWithHealthCheck appEnv (Proxy @API) handler identity identity context releaseAppEnv \flowRt -> do
    try (prepareRedisConnections $ appCfg.redisCfg)
      >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
    migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    pure flowRt
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. EmptyContext
