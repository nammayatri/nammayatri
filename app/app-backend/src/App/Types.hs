module App.Types
  ( Env,
    FlowHandler,
    FlowServer,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.SesConfig (SesConfig)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.App
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Credentials (PrivateKey)
import Beckn.Types.Flow
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
import ExternalAPI.Flow
import Types.Geofencing
import Types.Metrics

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    esqDBCfg :: EsqDBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    port :: Int,
    metricsPort :: Int,
    xProviderUri :: BaseUrl,
    hostName :: Text,
    bapSelfIds :: BAPs Text,
    bapSelfURIs :: BAPs BaseUrl,
    bapSelfUniqueKeyIds :: BAPs Text,
    searchConfirmExpiry :: Maybe Seconds,
    searchRequestExpiry :: Maybe Seconds,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
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
    graphhopperUrl :: BaseUrl,
    metricsSearchDurationTimeout :: Seconds,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    gatewayUrl :: BaseUrl
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    dbCfg :: DBConfig,
    esqDBEnv :: EsqDBEnv,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    xProviderUri :: BaseUrl,
    searchConfirmExpiry :: Maybe Seconds,
    searchRequestExpiry :: Maybe Seconds,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    domainVersion :: Text,
    geofencingConfig :: GeofencingConfig,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    apiRateLimitOptions :: APIRateLimitOptions,
    isShuttingDown :: TMVar (),
    bapMetrics :: BAPMetricsContainer,
    coreMetrics :: CoreMetricsContainer,
    authTokenCacheExpiry :: Seconds,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hostname <- getPodName
  isShuttingDown <- newEmptyTMVarIO
  bapMetrics <- registerBAPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.config.signingKey)
  getSignatureExpiry = (.config.signatureExpiry)

instance Registry Flow where
  registryLookup =
    Registry.withSubscriberCache $
      Registry.whitelisting isWhiteListed <=< Registry.registryLookup
    where
      isWhiteListed _ = pure True -- TODO: implement whitelisting

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Cache.getKey "taxi-bap:registry" . lookupRequestToRedisKey
  setKey = Cache.setKey "taxi-bap:registry" . lookupRequestToRedisKey
  delKey = Cache.delKey "taxi-bap:registry" . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx "taxi-bap:registry" ttl . lookupRequestToRedisKey
