{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API
import qualified Data.Map.Strict as Map
import "lib-dashboard" Environment
import qualified EulerHS.Runtime as R
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runServerWithHealthCheck)
import Servant (Context (..))
import qualified "lib-dashboard" Tools.Auth as Auth

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "bap-dashboard" <&> configModifier
  appEnv <- buildAppEnv authTokenCacheKeyPrefix appCfg
  -- Metrics.serve (appCfg.metricsPort) --  do we need it?
  runServerWithHealthCheck appEnv (Proxy @API) handler identity identity context releaseAppEnv \flowRt -> do
    migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    let flowRt' = flowRt {R._httpClientManagers = Map.singleton "default" (R._defaultHttpClientManager flowRt)}
    pure flowRt'
  where
    context =
      Auth.verifyApiAction @(FlowR AppEnv)
        :. Auth.verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext

    authTokenCacheKeyPrefix :: Text
    authTokenCacheKeyPrefix = "BAP-dashboard:authTokenCacheKey:"
