{-# OPTIONS_GHC -Wno-orphans #-}

module Environment where

import Beckn.External.Encryption (EncTools)
import Beckn.Sms.Config
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Redis.Config as T
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.App
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Credentials (PrivateKey)
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Registry
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.CacheRedis as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
import EulerHS.Prelude
import System.Environment (lookupEnv)
import Tools.Metrics.TransporterBPPMetrics.Types

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    nwAddress :: BaseUrl,
    selfUIUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    registryUrl :: BaseUrl,
    updateLocationRefreshPeriod :: Seconds,
    updateLocationAllowedDelay :: Seconds,
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    disableSignatureAuth :: Bool,
    otpSmsTemplate :: Text,
    smsCfg :: SmsConfig,
    inviteSmsTemplate :: Text,
    apiRateLimitOptions :: APIRateLimitOptions,
    driverPositionInfoExpiry :: Maybe Seconds,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    defaultRadiusOfSearch :: Meters,
    redisCfg :: T.RedisConfig,
    searchRequestExpirationSeconds :: Int,
    driverQuoteExpirationSeconds :: Int,
    httpClientOptions :: HttpClientOptions
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hostName :: Text,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    selfUIUrl :: BaseUrl,
    signatureExpiry :: Seconds,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    registryUrl :: BaseUrl,
    updateLocationRefreshPeriod :: Seconds,
    updateLocationAllowedDelay :: Seconds,
    disableSignatureAuth :: Bool,
    esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    isShuttingDown :: TMVar (),
    loggerEnv :: LoggerEnv,
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    port :: Int,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    otpSmsTemplate :: Text,
    smsCfg :: SmsConfig,
    inviteSmsTemplate :: Text,
    apiRateLimitOptions :: APIRateLimitOptions,
    driverPositionInfoExpiry :: Maybe Seconds,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    defaultRadiusOfSearch :: Meters,
    transporterMetrics :: TransporterMetricsContainer,
    searchRequestExpirationSeconds :: Int,
    driverQuoteExpirationSeconds :: Int
  }
  deriving (Generic)

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  isShuttingDown <- newEmptyTMVarIO
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  let modifierFunc = ("driver-offer-bpp:" <>)
  hedisEnv <- connectHedis hedisCfg modifierFunc
  coreMetrics <- Metrics.registerCoreMetricsContainer
  transporterMetrics <- registerTransporterMetricsContainer
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  -- FIXME: disconnect database?
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance Registry Flow where
  registryLookup = Registry.withSubscriberCache Registry.registryLookup

cacheRegistryKey :: Text
cacheRegistryKey = "driver-offer-bpp:registry"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Cache.getKey cacheRegistryKey . lookupRequestToRedisKey
  setKey = Cache.setKey cacheRegistryKey . lookupRequestToRedisKey
  delKey = Cache.delKey cacheRegistryKey . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx cacheRegistryKey ttl . lookupRequestToRedisKey
