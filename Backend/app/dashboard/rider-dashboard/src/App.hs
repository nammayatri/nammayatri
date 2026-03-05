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
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HMS
import "lib-dashboard" Environment
import EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDashboard)
import Kernel.Beam.Connection.Types (ConnectionConfigDashboard (..))
import Kernel.Beam.Types (KafkaConn (..), Tables (..))
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import Kernel.Types.Beckn.City (initCityMaps)
import Kernel.Types.Flow
import Kernel.Types.Time (getCurrentTime)
import Kernel.Utils.App
import qualified Kernel.Utils.Common as KUC
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runServerWithHealthCheckAndSlackNotification)
import qualified Network.Wai as Wai
import Servant (Context (..))
import qualified "lib-dashboard" Tools.Auth as Auth

requestArrivalLoggingMiddleware :: Wai.Middleware
requestArrivalLoggingMiddleware nextApp req respond = do
  arrivalTime <- getCurrentTime
  let requestIdText = (maybe "NO-REQUEST-ID" decodeUtf8 $ lookup "x-request-id" (Wai.requestHeaders req)) :: Text
      path = decodeUtf8 (Wai.rawPathInfo req) :: Text
      method = decodeUtf8 (Wai.requestMethod req) :: Text
      logMessage = ("[REQUEST-ARRIVAL] method=" <> method <> " path=" <> path <> " event=request_received_from_sidecar") :: Text
      logJson =
        object
          [ "timestamp" .= (show arrivalTime :: String),
            "requestId" .= requestIdText,
            "log" .= logMessage
          ]
  LBS.putStrLn $ A.encode logJson
  nextApp req respond

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "rider-dashboard" <&> configModifier
  appEnv <- buildAppEnv authTokenCacheKeyPrefix appCfg
  -- Metrics.serve (appCfg.metricsPort) --  do we need it?
  runServerWithHealthCheckAndSlackNotification appEnv (Proxy @API) handler requestArrivalLoggingMiddleware identity context releaseAppEnv \flowRt -> do
    prepareConnectionDashboard
      ( ConnectionConfigDashboard
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
    initCityMaps
    let flowRt' = flowRt {R._httpClientManagers = HMS.singleton "default" (R._defaultHttpClientManager flowRt)}
    pure flowRt'
  where
    context =
      Auth.verifyApiAction @(FlowR AppEnv)
        :. Auth.verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext

    authTokenCacheKeyPrefix :: Text
    authTokenCacheKeyPrefix = "rider-dashboard:authTokenCacheKey:"
