{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App
  ( runService,
  )
where

import API
import qualified Data.HashMap.Strict as HMS
import "lib-dashboard" Environment
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Beam.Types (KafkaConn (..), Tables (..))
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import Kernel.Types.Flow
import Kernel.Types.Logging (LogLevel (..))
import Kernel.Utils.App
import qualified Kernel.Utils.Common as KUC
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.IOLogging (logOutputIO)
import Kernel.Utils.Servant.Server (runServerWithHealthCheckAndSlackNotification)
import qualified Kernel.Tools.Metrics.Init as Metrics
import Network.HTTP.Types (status408)
import qualified Network.Wai as Wai
import Servant (Context (..))
import Storage.Beam.SystemConfigs ()
import System.Timeout (timeout)
import qualified "lib-dashboard" Tools.Auth as Auth
import qualified Tools.Auth.Webhook as AuthWebhook

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "safety-dashboard" <&> configModifier
  appEnv <- buildAppEnv authTokenCacheKeyPrefix appCfg
  Metrics.serve (appCfg.metricsPort)
  runServerWithHealthCheckAndSlackNotification appEnv (Proxy @API) handler (dashboardTimeoutMiddleware appEnv appCfg.incomingAPIResponseTimeout . logIncomingRequest appEnv) identity context releaseAppEnv \flowRt -> do
    prepareConnectionRider
      ( ConnectionConfigRider
          { esqDBCfg = appCfg.esqDBCfg,
            esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
            hedisClusterCfg = appCfg.hedisClusterCfg,
            hedisSecondaryClusterCfg = appCfg.hedisSecondaryClusterCfg
          }
      )
      appCfg.kvConfigUpdateFrequency
    L.setOption KafkaConn appEnv.kafkaProducerTools
    L.setOption Tables KUC.defaultTableData
    migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    let flowRt' = flowRt {R._httpClientManagers = HMS.singleton "default" (R._defaultHttpClientManager flowRt)}
    pure flowRt'
  where
    context =
      Auth.verifyApiAction @(FlowR AppEnv)
        :. Auth.verifyDashboardAction @(FlowR AppEnv)
        :. AuthWebhook.verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext

    logIncomingRequest :: AppEnv -> Wai.Middleware
    logIncomingRequest env waiApp req respond = do
      let reqId = maybe "N/A" decodeUtf8 $ lookup "x-request-id" (Wai.requestHeaders req)
          path = decodeUtf8 $ Wai.rawPathInfo req
          method = decodeUtf8 $ Wai.requestMethod req
      logOutputIO env.loggerEnv INFO ("Incoming dashboard request | requestId: " <> reqId <> " | " <> method <> " " <> path) (Just reqId) Nothing
      waiApp req respond

    dashboardTimeoutMiddleware :: AppEnv -> Int -> Wai.Middleware
    dashboardTimeoutMiddleware env seconds waiApp req respond = do
      result <- timeout (seconds * 1000000) (waiApp req respond)
      case result of
        Just response -> pure response
        Nothing -> do
          let reqId = maybe "N/A" decodeUtf8 $ lookup "x-request-id" (Wai.requestHeaders req)
              path = decodeUtf8 $ Wai.rawPathInfo req
              query = decodeUtf8 $ Wai.rawQueryString req
          logOutputIO env.loggerEnv ERROR ("Request timed out! requestId: " <> reqId <> " | Path: " <> path <> " | Query: " <> query <> " | Timeout: " <> show seconds <> " seconds") (Just reqId) Nothing
          respond $ Wai.responseLBS status408 [] ""

    authTokenCacheKeyPrefix :: Text
    authTokenCacheKeyPrefix = "safety-dashboard:authTokenCacheKey:"
