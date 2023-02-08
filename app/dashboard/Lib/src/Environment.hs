module Environment where

import Kernel.External.Encryption (EncTools)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, connectHedis, disconnectHedis)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client
import Kernel.Utils.Shutdown
import Tools.Client
import Tools.Metrics

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    port :: Int,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    registrationTokenExpiry :: Days,
    encTools :: EncTools,
    dataServers :: [DataServer]
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    port :: Int,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    registrationTokenExpiry :: Days,
    encTools :: EncTools,
    coreMetrics :: Metrics.CoreMetricsContainer,
    isShuttingDown :: Shutdown,
    authTokenCacheKeyPrefix :: Text,
    dataServers :: [DataServer]
  }
  deriving (Generic)

buildAppEnv :: Text -> AppCfg -> IO AppEnv
buildAppEnv authTokenCacheKeyPrefix AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  coreMetrics <- registerCoreMetricsContainer
  let modifierFunc = ("dashboard:" <>)
  hedisEnv <- connectHedis hedisCfg modifierFunc
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
