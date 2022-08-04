module App.Types
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
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis
import Beckn.Storage.Hedis.AppPrefixes
import Beckn.Types.App
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Credentials (PrivateKey)
import Beckn.Types.Flow
import Beckn.Types.Geofencing
import Beckn.Types.Registry
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.CacheRedis as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as T
import System.Environment (lookupEnv)
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    redisCfg :: T.RedisConfig,
    hcfg :: HedisCfg,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
    port :: Int,
    bgtmPort :: Int,
    metricsPort :: Int,
    hostName :: Text,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    caseExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    geofencingConfig :: GeofencingConfig,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmTokenKeyPrefix :: Text,
    graphhopperUrl :: BaseUrl,
    graceTerminationPeriod :: Seconds,
    defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    recalculateFareEnabled :: Bool,
    updateLocationRefreshPeriod :: Seconds,
    updateLocationAllowedDelay :: Seconds,
    metricsSearchDurationTimeout :: Seconds,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets,
    disableSignatureAuth :: Bool,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    selfUIUrl :: BaseUrl,
    schedulingReserveTime :: Seconds
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
    bgtmPort :: Int,
    hostName :: Text,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    caseExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    geofencingConfig :: GeofencingConfig,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    fcmJsonPath :: Maybe Text,
    fcmTokenKeyPrefix :: Text,
    graphhopperUrl :: BaseUrl,
    graceTerminationPeriod :: Seconds,
    defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    recalculateFareEnabled :: Bool,
    updateLocationRefreshPeriod :: Seconds,
    updateLocationAllowedDelay :: Seconds,
    metricsSearchDurationTimeout :: Seconds,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets,
    disableSignatureAuth :: Bool,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    selfUIUrl :: BaseUrl,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: TMVar (),
    bppMetrics :: BPPMetricsContainer,
    coreMetrics :: CoreMetricsContainer,
    transporterMetrics :: TransporterMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools,
    kafkaEnvs :: BPPKafkaEnvs,
    hedisEnv :: HedisEnv,
    schedulingReserveTime :: Seconds
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  transporterMetrics <- registerTransporterMetricsContainer
  isShuttingDown <- newEmptyTMVarIO
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  kafkaEnvs <- buildBPPKafkaEnvs
  hedisEnv <- connectHedis hcfg becknTransportPrefix
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
  getKey = Cache.getKey "taxi-bpp:registry" . lookupRequestToRedisKey
  setKey = Cache.setKey "taxi-bpp:registry" . lookupRequestToRedisKey
  delKey = Cache.delKey "taxi-bpp:registry" . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx "taxi-bpp:registry" ttl . lookupRequestToRedisKey
