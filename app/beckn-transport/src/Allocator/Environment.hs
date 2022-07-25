module Allocator.Environment where

import qualified App.Types as App
import Beckn.External.Encryption (EncTools)
import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import EulerHS.Prelude
import EulerHS.Types (RedisConfig)
import Tools.Metrics
import Tools.Streaming.Kafka
import Types.App (SortMode)
import Types.Shard
import Utils.Common

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { appCfg :: App.AppCfg,
    esqDBCfg :: EsqDBConfig,
    redisCfg :: RedisConfig,
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
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    exotelCfg :: Maybe ExotelCfg,
    defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    graceTerminationPeriod :: Seconds,
    encTools :: EncTools,
    selfUIUrl :: BaseUrl
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { appCfg :: App.AppCfg, --should we flatten it too?
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
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    exotelCfg :: Maybe ExotelCfg,
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
    selfUIUrl :: BaseUrl
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
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.appCfg.signingKey)
  getSignatureExpiry = (.appCfg.signatureExpiry)
