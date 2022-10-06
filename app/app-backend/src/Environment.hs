module Environment
  ( Env,
    FlowHandler,
    FlowServer,
    Flow,
    AppCfg (),
    AppEnv (..),
    BAPs (..),
    HasBapInfo,
    RideConfig (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import Beckn.External.Encryption (EncTools)
import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis.AppPrefixes (appBackendPrefix)
import Beckn.Storage.Hedis.Config
import Beckn.Types.App
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Credentials (PrivateKey)
import Beckn.Types.Flow
import Beckn.Types.Id (ShortId (..))
import Beckn.Types.Registry
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.App (getPodName)
import Beckn.Utils.CacheRedis as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Storage.CachedQueries.CacheConfig
import Storage.CachedQueries.Organization (findByShortId)
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    redisCfg :: T.RedisConfig,
    hedisCfg :: HedisCfg,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    bapSelfIds :: BAPs Text,
    bapSelfURIs :: BAPs BaseUrl,
    bapSelfUniqueKeyIds :: BAPs Text,
    searchRequestExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    metricsSearchDurationTimeout :: Seconds,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    registryUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    gatewayUrl :: BaseUrl,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    selfUIUrl :: BaseUrl,
    rideCfg :: RideConfig,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig
  }
  deriving (Generic, FromDhall)

-- TODO coreVersion should be hardcoded in spec, because we can't change coreVersion without changing code
data AppEnv = AppEnv
  { smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    hostName :: Text,
    bapSelfIds :: BAPs Text,
    bapSelfURIs :: BAPs BaseUrl,
    searchRequestExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    registryUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    gatewayUrl :: BaseUrl,
    encTools :: EncTools,
    selfUIUrl :: BaseUrl,
    hedisEnv :: HedisEnv,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: TMVar (),
    bapMetrics :: BAPMetricsContainer,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools,
    kafkaEnvs :: BAPKafkaEnvs,
    rideCfg :: RideConfig,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- getPodName
  isShuttingDown <- newEmptyTMVarIO
  bapMetrics <- registerBAPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  kafkaEnvs <- buildBAPKafkaEnvs
  hedisEnv <- connectHedis hedisCfg appBackendPrefix
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

data BAPs a = BAPs
  { metro :: a,
    cabs :: a
  }
  deriving (Generic, FromDhall)

type HasBapInfo r m =
  ( HasField "bapSelfIds" r (BAPs Text),
    HasField "bapSelfURIs" r (BAPs BaseUrl),
    MonadReader r m
  )

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

instance Registry Flow where
  registryLookup registryUrl =
    Registry.withSubscriberCache $
      Registry.whitelisting isWhiteListed <=< Registry.registryLookup registryUrl
    where
      isWhiteListed subscriberId = findByShortId (ShortId subscriberId) <&> isJust

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Cache.getKey "taxi-bap:registry" . lookupRequestToRedisKey
  setKey = Cache.setKey "taxi-bap:registry" . lookupRequestToRedisKey
  delKey = Cache.delKey "taxi-bap:registry" . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx "taxi-bap:registry" ttl . lookupRequestToRedisKey

data RideConfig = RideConfig
  { driverReachedDistance :: Meters,
    driverOnTheWayNotifyExpiry :: Seconds
  }
  deriving (Generic, FromDhall)
