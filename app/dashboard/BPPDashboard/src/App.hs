{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API
import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import qualified Data.Map.Strict as Map
import "lib-dashboard" Environment
import qualified EulerHS.Runtime as R
import Servant (Context (..))
import qualified "lib-dashboard" Tools.Auth as Auth

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "bpp-dashboard" <&> configModifier
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
        :. Auth.verifyServerAction @(FlowR AppEnv)
        :. EmptyContext

    authTokenCacheKeyPrefix :: Text
    authTokenCacheKeyPrefix = "BPP-dashboard:authTokenCacheKey:"
