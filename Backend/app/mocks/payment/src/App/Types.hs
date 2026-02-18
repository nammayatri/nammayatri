module App.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.IORef
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import qualified Kernel.Types.CacheFlow as CF
import Kernel.Types.Common hiding (id)
import Kernel.Types.Flow
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import Lib.Payment.Storage.Beam.PaymentOrder ()
import Lib.Payment.Storage.Beam.PaymentOrderOffer ()
import Lib.Payment.Storage.Beam.PaymentOrderSplit ()
import Lib.Payment.Storage.Beam.PaymentTransaction ()
import Lib.Payment.Storage.Beam.PayoutOrder ()
import Lib.Payment.Storage.Beam.PayoutTransaction ()
import Lib.Payment.Storage.Beam.PersonWallet ()
import Lib.Payment.Storage.Beam.Refunds ()
import Lib.Payment.Storage.Beam.WalletRewardPosting ()
import System.Environment (lookupEnv)

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    juspayWebhookBaseUrl :: BaseUrl,
    esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    juspayWebhookBaseUrl :: BaseUrl,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    version :: Metrics.DeploymentVersion,
    requestId :: Maybe Text,
    sessionId :: Maybe Text,
txnId :: Maybe Text,
    url :: Maybe Text,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    secondaryHedisClusterEnv :: Maybe HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    coreMetrics :: Metrics.CoreMetricsContainer,
    shouldLogRequestId :: Bool,
    hedisMigrationStage :: Bool,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    kafkaProducerForART :: Maybe Kernel.Streaming.Kafka.Producer.Types.KafkaProducerTools,
    cacheConfig :: CF.CacheConfig,
    cacConfig :: CF.CacConfig,
    inMemEnv :: CF.InMemEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  isShuttingDown <- mkShutdown
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  let esqDBReplicaEnv = esqDBEnv
  hedisEnv <- connectHedis hedisCfg ("mock-payment:" <>)
  let hedisClusterEnv = hedisEnv
  let secondaryHedisClusterEnv = Nothing
  let hedisNonCriticalEnv = hedisEnv
  let hedisNonCriticalClusterEnv = hedisEnv
  coreMetrics <- Metrics.registerCoreMetricsContainer
  let requestId = Nothing
  let sessionId = Nothing
      txnId = Nothing
  let url = Nothing
  let shouldLogRequestId = False
  let hedisMigrationStage = False
  let enableRedisLatencyLogging = False
  let enablePrometheusMetricLogging = False
  let kafkaProducerForART = Nothing

  let cacheConfig = CF.CacheConfig {configsExpTime = 300}
  let cacConfig = CF.CacConfig {host = "localhost", interval = 10, tenant = "mock", retryConnection = False, cacExpTime = 300, enablePolling = False, enableCac = False}

  now <- getCurrentTime
  inMemHashMap <- Data.IORef.newIORef $ CF.InMemCacheInfo {cache = HM.empty, cacheSize = 0, createdAt = now}
  let inMemEnv = CF.InMemEnv {enableInMem = True, maxInMemSize = 1024 * 1024, inMemHashMap = inMemHashMap}

  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
