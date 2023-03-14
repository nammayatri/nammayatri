{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment
  ( AppCfg (),
    AppEnv (..),
    Env,
    FlowHandler,
    FlowServer,
    Flow,
    Log (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.UI.Ride.EndRide.DefaultConfig as EndRideDefCfg
import EulerHS.Prelude
import Kernel.External.Encryption (EncTools)
import Kernel.External.Exotel.Types (ExotelCfg)
import Kernel.External.Infobip.Types (InfoBIPConfig, WebengageConfig)
import Kernel.Sms.Config
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis
import Kernel.Storage.Hedis.AppPrefixes
import Kernel.Types.App
import Kernel.Types.Cache
import Kernel.Types.Common
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Flow
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Servant.SignatureAuth
import SharedLogic.DriverPool (DriverPoolConfig)
import Storage.CachedQueries.CacheConfig
import System.Environment (lookupEnv)
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    clickhouseCfg :: ClickhouseCfg,
    smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    webengageCfg :: WebengageConfig,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    searchExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    recalculateFareEnabled :: Bool,
    metricsSearchDurationTimeout :: Seconds,
    registryUrl :: BaseUrl,
    disableSignatureAuth :: Bool,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    selfUIUrl :: BaseUrl,
    schedulingReserveTime :: Seconds,
    driverEstimatedPickupDuration :: Seconds,
    dashboardToken :: Text,
    defaultEndRideCfg :: EndRideDefCfg.EndRideDefaultConfig,
    cacheConfig :: CacheConfig,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverReachedDistance :: HighPrecMeters,
    driverPoolCfg :: DriverPoolConfig,
    driverLocationUpdateTopic :: Text,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    appPrefix :: Text
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    webengageCfg :: WebengageConfig,
    hostName :: Text,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    searchExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    recalculateFareEnabled :: Bool,
    registryUrl :: BaseUrl,
    disableSignatureAuth :: Bool,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    selfUIUrl :: BaseUrl,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    clickhouseEnv :: ClickhouseEnv,
    isShuttingDown :: TMVar (),
    bppMetrics :: BPPMetricsContainer,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools,
    kafkaEnvs :: BPPKafkaEnvs,
    hedisEnv :: HedisEnv,
    schedulingReserveTime :: Seconds,
    driverEstimatedPickupDuration :: Seconds,
    dashboardToken :: Text,
    defaultEndRideCfg :: EndRideDefCfg.EndRideDefaultConfig,
    cacheConfig :: CacheConfig,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverReachedDistance :: HighPrecMeters,
    driverPoolCfg :: DriverPoolConfig,
    driverLocationUpdateTopic :: Text,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    appPrefix :: Text
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- newEmptyTMVarIO
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  clickhouseEnv <- createConn clickhouseCfg
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  kafkaEnvs <- buildBPPKafkaEnvs
  hedisEnv <- connectHedis hedisCfg staticOfferDriverAppPrefix
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  disconnectHedis hedisEnv
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

instance Registry Flow where
  registryLookup registryUrl = Registry.withSubscriberCache $ Registry.registryLookup registryUrl

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . ("taxi-bpp:registry:" <>) . lookupRequestToRedisKey
  setKey = Redis.set . ("taxi-bpp:registry:" <>) . lookupRequestToRedisKey
  delKey = Redis.del . ("taxi-bpp:registry:" <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . ("taxi-bpp:registry:" <>) . lookupRequestToRedisKey
