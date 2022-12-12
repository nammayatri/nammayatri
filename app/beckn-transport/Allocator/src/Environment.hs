{-# LANGUAGE PackageImports #-}

module Environment where

import Beckn.External.Encryption (EncTools)
import Beckn.Storage.Esqueleto.Config
import qualified Beckn.Storage.Hedis as Redis
import qualified Beckn.Storage.Hedis.AppPrefixes as Redis
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id (ShortId)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import Domain.Types.Merchant (Subscriber)
import qualified "beckn-transport" Environment as App
import EulerHS.Prelude
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
    driverPoolCfg :: DriverPoolConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { appCfg :: App.AppCfg,
    httpClientOptions :: HttpClientOptions,
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
    driverPoolCfg :: DriverPoolConfig
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
  hedisEnv <- Redis.connectHedis hedisCfg Redis.becknTransportPrefix
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  Redis.disconnectHedis hedisEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.appCfg.signingKey)
  getSignatureExpiry = (.appCfg.signatureExpiry)
