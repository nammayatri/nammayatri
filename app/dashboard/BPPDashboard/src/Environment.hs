module Environment where

import Beckn.External.Encryption (EncTools)
import Beckn.Prelude
import Beckn.Sms.Config
import Beckn.Storage.Esqueleto.Config
import qualified Beckn.Storage.Redis.Config as Redis
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client
import Beckn.Utils.Shutdown
import Tools.Metrics

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    redisCfg :: Redis.RedisConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    port :: Int,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    encTools :: EncTools
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { graceTerminationPeriod :: Seconds,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    port :: Int,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    apiRateLimitOptions :: APIRateLimitOptions
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
