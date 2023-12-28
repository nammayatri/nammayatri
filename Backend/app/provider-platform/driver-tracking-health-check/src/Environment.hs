{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import EulerHS.Prelude
import Kernel.External.Encryption (EncTools)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Common (CacheConfig)
import Kernel.Utils.Dhall
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Shutdown
import Tools.Metrics

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    healthcheckPort :: Int,
    driverAppName :: Text,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    graceTerminationPeriod :: Seconds,
    hedisCfg :: Redis.HedisCfg,
    hedisClusterCfg :: Redis.HedisCfg,
    hedisNonCriticalCfg :: Redis.HedisCfg,
    hedisNonCriticalClusterCfg :: Redis.HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    esqDBCfg :: EsqDBConfig,
    encTools :: EncTools,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    driverAllowedDelayForLocationUpdateInSec :: Seconds,
    driverLocationHealthCheckIntervalInSec :: Seconds,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text,
    cacheConfig :: CacheConfig,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    httpClientOptions :: HttpClientOptions,
    driverAppName :: Text,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    driverAllowedDelayForLocationUpdateInSec :: Seconds,
    driverLocationHealthCheckIntervalInSec :: Seconds,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text,
    esqDBEnv :: EsqDBEnv,
    hedisEnv :: Redis.HedisEnv,
    hedisNonCriticalEnv :: Redis.HedisEnv,
    hedisNonCriticalClusterEnv :: Redis.HedisEnv,
    hedisClusterEnv :: Redis.HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    cacheConfig :: CacheConfig,
    version :: DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- mkShutdown
  hostname <- getPodName
  version <- lookupDeploymentVersion
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  let modifierFunc = (driverAppName <>)
  hedisEnv <- Redis.connectHedis hedisCfg modifierFunc
  hedisNonCriticalEnv <- Redis.connectHedis hedisNonCriticalCfg modifierFunc
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else Redis.connectHedisCluster hedisClusterCfg modifierFunc
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else Redis.connectHedisCluster hedisNonCriticalClusterCfg modifierFunc
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  Redis.disconnectHedis hedisEnv
  Redis.disconnectHedis hedisClusterEnv
