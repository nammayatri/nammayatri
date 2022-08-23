module Environment where

import Beckn.External.Encryption (EncTools)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Redis.Config (RedisConfig)
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Shutdown
import EulerHS.Prelude
import Tools.Metrics

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    healthcheckPort :: Int,
    httpClientOptions :: HttpClientOptions,
    graceTerminationPeriod :: Seconds,
    redisCfg :: RedisConfig,
    esqDBCfg :: EsqDBConfig,
    fcmUrl :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmTokenKeyPrefix :: Text,
    encTools :: EncTools,
    driverAllowedDelay :: Seconds,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    httpClientOptions :: HttpClientOptions,
    graceTerminationPeriod :: Seconds,
    fcmUrl :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmTokenKeyPrefix :: Text,
    encTools :: EncTools,
    driverAllowedDelay :: Seconds,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- mkShutdown
  hostname <- getPodName
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
