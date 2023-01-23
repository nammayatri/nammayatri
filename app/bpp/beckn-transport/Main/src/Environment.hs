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

import Beckn.External.Encryption (EncTools)
import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.External.Infobip.Types (InfoBIPConfig, WebengageConfig)
import Beckn.Sms.Config
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis as Redis
import Beckn.Storage.Hedis.AppPrefixes
import Beckn.Types.App
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Credentials (PrivateKey)
import Beckn.Types.Flow
import Beckn.Types.Registry
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
import EulerHS.Prelude
import SharedLogic.DriverPool (DriverPoolConfig)
import Storage.CachedQueries.CacheConfig
import System.Environment (lookupEnv)
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    webengageCfg :: WebengageConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
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
    defaultPickupLocThreshold :: Meters,
    defaultDropLocThreshold :: Meters,
    defaultrideTravelledDistThresholdWhenPickupOrDestIsDiff :: Meters,
    defaultrideTravelledDistThresholdWhenPickupAndDestIsSame :: Meters,
    defaultRideTimeEstimatedThreshold :: Seconds,
    defaultWaitingTimeEstimatedThreshold :: Seconds,
    cacheConfig :: CacheConfig,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverLocationUpdateNotificationTemplate :: Text,
    driverReachedDistance :: HighPrecMeters,
    driverPoolCfg :: DriverPoolConfig,
    driverLocationUpdateTopic :: Text
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    webengageCfg :: WebengageConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
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
    defaultPickupLocThreshold :: Meters,
    defaultDropLocThreshold :: Meters,
    defaultrideTravelledDistThresholdWhenPickupOrDestIsDiff :: Meters,
    defaultrideTravelledDistThresholdWhenPickupAndDestIsSame :: Meters,
    defaultRideTimeEstimatedThreshold :: Seconds,
    defaultWaitingTimeEstimatedThreshold :: Seconds,
    cacheConfig :: CacheConfig,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverLocationUpdateNotificationTemplate :: Text,
    driverReachedDistance :: HighPrecMeters,
    driverPoolCfg :: DriverPoolConfig,
    driverLocationUpdateTopic :: Text
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
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  kafkaEnvs <- buildBPPKafkaEnvs
  hedisEnv <- connectHedis hedisCfg becknTransportPrefix
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
