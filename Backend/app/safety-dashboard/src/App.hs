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
import Kernel.Utils.App
import qualified Kernel.Utils.Common as KUC
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runServerWithHealthCheckAndSlackNotification)
import Servant (Context (..))
import Storage.Beam.SystemConfigs ()
import qualified "lib-dashboard" Tools.Auth as Auth
import qualified Tools.Auth.Webhook as AuthWebhook

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "safety-dashboard" <&> configModifier
  appEnv <- buildAppEnv authTokenCacheKeyPrefix appCfg
  runServerWithHealthCheckAndSlackNotification appEnv (Proxy @API) handler identity identity context releaseAppEnv \flowRt -> do
    prepareConnectionRider
      ( ConnectionConfigRider
          { esqDBCfg = appCfg.esqDBCfg,
            esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
            hedisClusterCfg = appCfg.hedisClusterCfg
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

    authTokenCacheKeyPrefix :: Text
    authTokenCacheKeyPrefix = "safety-dashboard:authTokenCacheKey:"
