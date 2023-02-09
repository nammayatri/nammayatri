{-# LANGUAGE PackageImports #-}

module Environment where

import Domain.Action.Allocation.Internal.DriverPool.Config (DriverPoolBatchesConfig)
import Domain.Types.Merchant (Subscriber)
import qualified "beckn-transport" Environment as App
import EulerHS.Prelude
import Kernel.External.Encryption (EncTools)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.AppPrefixes as Redis
import Kernel.Types.Common
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id (ShortId)
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth
import Kernel.Utils.Shutdown
import SharedLogic.DriverPool (DriverPoolConfig)
import Storage.CachedQueries.CacheConfig
import Tools.Metrics
import Tools.Streaming.Kafka

type Flow = FlowR AppEnv

type Shards = Map Int (ShortId Subscriber)

data AppCfg = AppCfg
  { appCfg :: App.AppCfg,
    esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: Redis.HedisCfg,
    metricsPort :: Int,
    healthcheckPort :: Int,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    driverNotificationExpiry :: Seconds,
    rideAllocationExpiry :: Seconds,
    reallocationsLimit :: Int,
    requestsNumPerIteration :: Integer,
    processDelay :: Milliseconds,
    shards :: Shards,
    loggerConfig :: LoggerConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    nwAddress :: BaseUrl,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    selfUIUrl :: BaseUrl,
    cacheConfig :: CacheConfig,
    driverPoolCfg :: DriverPoolConfig,
    driverPoolBatchesCfg :: DriverPoolBatchesConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { appCfg :: App.AppCfg,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    driverNotificationExpiry :: Seconds,
    rideAllocationExpiry :: Seconds,
    reallocationsLimit :: Int,
    requestsNumPerIteration :: Integer,
    processDelay :: Milliseconds,
    shards :: Shards,
    loggerConfig :: LoggerConfig,
    nwAddress :: BaseUrl,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    hedisEnv :: Redis.HedisEnv,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    btmMetrics :: AllocatorMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools,
    selfUIUrl :: BaseUrl,
    cacheConfig :: CacheConfig,
    driverPoolCfg :: DriverPoolConfig,
    driverPoolBatchesCfg :: DriverPoolBatchesConfig
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- mkShutdown
  btmMetrics <- registerAllocatorMetricsContainer
  hostname <- getPodName
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  hedisEnv <- Redis.connectHedis hedisCfg Redis.staticOfferDriverAppPrefix
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  Redis.disconnectHedis hedisEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.appCfg.signingKey)
  getSignatureExpiry = (.appCfg.signatureExpiry)
