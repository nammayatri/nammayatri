module Environment where

import EulerHS.Prelude
import Kernel.External.Encryption (EncTools)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Dhall
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Shutdown
import Storage.CachedQueries.CacheConfig
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
    esqDBCfg :: EsqDBConfig,
    encTools :: EncTools,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    driverAllowedDelayForLocationUpdateInSec :: Seconds,
    driverLocationHealthCheckIntervalInSec :: Seconds,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text,
    cacheConfig :: CacheConfig
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
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    cacheConfig :: CacheConfig
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- mkShutdown
  hostname <- getPodName
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  let modifierFunc = (driverAppName <>)
  hedisEnv <- Redis.connectHedis hedisCfg modifierFunc
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  Redis.disconnectHedis hedisEnv
