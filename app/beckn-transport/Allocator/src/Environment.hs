{-# LANGUAGE PackageImports #-}

module Environment where

import Beckn.External.Encryption (EncTools)
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis
import Beckn.Storage.Hedis.AppPrefixes
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id (ShortId)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import Domain.Action.Allocation (SortMode)
import Domain.Types.Organization (Organization)
import qualified "beckn-transport" Environment as App
import EulerHS.Prelude
import EulerHS.Types (RedisConfig)
import Storage.CachedQueries.CacheConfig
import Tools.Metrics
import Tools.Streaming.Kafka

type Flow = FlowR AppEnv

type Shards = Map Int (ShortId Organization)

data AppCfg = AppCfg
  { appCfg :: App.AppCfg,
    esqDBCfg :: EsqDBConfig,
    redisCfg :: RedisConfig,
    hedisCfg :: HedisCfg,
    metricsPort :: Int,
    healthcheckPort :: Int,
    httpClientOptions :: HttpClientOptions,
    driverNotificationExpiry :: Seconds,
    rideAllocationExpiry :: Seconds,
    defaultSortMode :: SortMode,
    driverBatchSize :: Int,
    reallocationsLimit :: Int,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    requestsNumPerIteration :: Integer,
    processDelay :: Milliseconds,
    shards :: Shards,
    loggerConfig :: LoggerConfig,
    kafkaProducerCfg :: KafkaProducerCfg,
    nwAddress :: BaseUrl,
    fcmUrl :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmTokenKeyPrefix :: Text,
    defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    selfUIUrl :: BaseUrl,
    cacheConfig :: CacheConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { appCfg :: App.AppCfg,
    httpClientOptions :: HttpClientOptions,
    driverNotificationExpiry :: Seconds,
    rideAllocationExpiry :: Seconds,
    defaultSortMode :: SortMode,
    driverBatchSize :: Int,
    reallocationsLimit :: Int,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    requestsNumPerIteration :: Integer,
    processDelay :: Milliseconds,
    shards :: Shards,
    loggerConfig :: LoggerConfig,
    nwAddress :: BaseUrl,
    fcmUrl :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmTokenKeyPrefix :: Text,
    defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    btmMetrics :: AllocatorMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools,
    hedisEnv :: HedisEnv,
    selfUIUrl :: BaseUrl,
    cacheConfig :: CacheConfig
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
  hedisEnv <- connectHedis hedisCfg becknTransportPrefix
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  disconnectHedis hedisEnv
  releaseLoggerEnv loggerEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.appCfg.signingKey)
  getSignatureExpiry = (.appCfg.signatureExpiry)
