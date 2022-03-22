module App.Allocator.Config where

import qualified App.Types as App
import Beckn.External.Encryption (EncTools)
import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Storage.Esqueleto.Config (EsqDBConfig)
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import EulerHS.Types (RedisConfig)
import Tools.Streaming.Kafka
import Types.App (SortMode)
import Types.Shard
import Utils.Common

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
    encTools :: EncTools
  }
  deriving (Generic, FromDhall)
