{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API (healthCheckAPI, healthCheck, iAmAlive) where

import EulerHS.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import Servant (Get, JSON)
import Storage.Beam.SystemConfigs ()
import Tools.Error
import Tools.Metrics (HasCoreMetrics)

type HealthCheckAPI = Get '[JSON] Text

healthCheckAPI :: Proxy HealthCheckAPI
healthCheckAPI = Proxy

healthCheck ::
  ( HasCoreMetrics r,
    HasField "isShuttingDown" r Shutdown,
    HasField "loggerEnv" r LoggerEnv,
    HasField "hedisEnv" r Redis.HedisEnv,
    HasField "hedisNonCriticalEnv" r Redis.HedisEnv,
    HasField "hedisNonCriticalClusterEnv" r Redis.HedisEnv,
    HasField "hedisClusterEnv" r Redis.HedisEnv,
    HasField "hedisMigrationStage" r Bool,
    HasField "driverAppName" r Text,
    HasField "enablePrometheusMetricLogging" r Bool,
    HasField "enableRedisLatencyLogging" r Bool
  ) =>
  FlowHandlerR r Text
healthCheck = withFlowHandlerAPI do
  redisPrefix <- asks (.driverAppName)
  mbTime <- Redis.get $ key redisPrefix
  maybe markAsDead checkLastUpdateTime mbTime
  where
    markAsDead = throwError ServiceUnavailable
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrentTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"

key :: Text -> Text
key prefix = prefix <> "driver-tracking-healthcheck:service"

--TODO: Make ServiceHealthChecker util in shared-kernel
iAmAlive :: (Redis.HedisFlow m r, HasField "driverAppName" r Text, MonadTime m) => m ()
iAmAlive = do
  redisPrefix <- asks (.driverAppName)
  getCurrentTime >>= Redis.set (key redisPrefix)
