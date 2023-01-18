module Environment where

import Beckn.External.Encryption (EncTools)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.Esqueleto.Config
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Beckn.Utils.Shutdown
import EulerHS.Prelude
import Storage.CachedQueries.CacheConfig
import Tools.Metrics

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    healthcheckPort :: Int,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    graceTerminationPeriod :: Seconds,
    hedisCfg :: Redis.HedisCfg,
    esqDBCfg :: EsqDBConfig,
    encTools :: EncTools,
    driverAllowedDelay :: Seconds,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text,
    cacheConfig :: CacheConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    driverAllowedDelay :: Seconds,
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
  let modifierFunc = ("beckn-transport:" <>)
  hedisEnv <- Redis.connectHedis hedisCfg modifierFunc
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  Redis.disconnectHedis hedisEnv
