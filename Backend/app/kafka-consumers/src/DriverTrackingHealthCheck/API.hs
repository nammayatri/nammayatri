{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DriverTrackingHealthCheck.API (healthCheckAPI, healthCheck, iAmAlive) where

import EulerHS.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (HasCoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import Servant (Get, JSON)

type HealthCheckAPI = Get '[JSON] Text

healthCheckAPI :: Proxy HealthCheckAPI
healthCheckAPI = Proxy

healthCheck ::
  ( HasCoreMetrics r,
    HasField "isShuttingDown" r Shutdown,
    HasField "loggerEnv" r LoggerEnv,
    HasField "hedisEnv" r Redis.HedisEnv,
    HasField "requestId" r (Maybe Text),
    HasField "sessionId" r (Maybe Text),
    HasField "hedisMigrationStage" r Bool,
    HasField "enableRedisLatencyLogging" r Bool,
    HasField "enablePrometheusMetricLogging" r Bool,
    HasField "hedisNonCriticalEnv" r Redis.HedisEnv,
    HasField "hedisNonCriticalClusterEnv" r Redis.HedisEnv,
    HasField "hedisClusterEnv" r Redis.HedisEnv
  ) =>
  FlowHandlerR r Text
healthCheck = withFlowHandlerAPI' do
  mbTime <- Redis.get key
  maybe markAsDead checkLastUpdateTime mbTime
  where
    markAsDead = throwError ServiceUnavailable
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrentTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"

key :: Text
key = "dynamic-offer-driver-app" <> "driver-tracking-healthcheck:service"

iAmAlive :: (Redis.HedisFlow m r, MonadTime m) => m ()
iAmAlive = do
  getCurrentTime >>= Redis.set key
