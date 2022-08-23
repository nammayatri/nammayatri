{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Environment where

import AWS.S3 (S3AuthenticatingEntity (..), S3Config)
import Beckn.External.Encryption (EncTools)
import Beckn.Prelude (NominalDiffTime)
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
    s3Config :: S3Config,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
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
    fcmTokenKeyPrefix :: Text,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    defaultRadiusOfSearch :: Meters,
    redisCfg :: T.RedisConfig,
    searchRequestExpirationSeconds :: Int,
    driverQuoteExpirationSeconds :: Int,
    httpClientOptions :: HttpClientOptions,
    driverUnlockDelay :: Seconds
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hostName :: Text,
    nwAddress :: BaseUrl,
    signingKey :: PrivateKey,
    selfUIUrl :: BaseUrl,
    signatureExpiry :: Seconds,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    s3Config :: S3Config,
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
    fcmTokenKeyPrefix :: Text,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    defaultRadiusOfSearch :: Meters,
    transporterMetrics :: TransporterMetricsContainer,
    searchRequestExpirationSeconds :: NominalDiffTime,
    driverQuoteExpirationSeconds :: NominalDiffTime,
    driverUnlockDelay :: Seconds
  }
  deriving (Generic)

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv cfg@AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  isShuttingDown <- newEmptyTMVarIO
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  let modifierFunc = ("driver-offer-bpp:" <>)
  hedisEnv <- connectHedis hedisCfg modifierFunc
  coreMetrics <- Metrics.registerCoreMetricsContainer
  transporterMetrics <- registerTransporterMetricsContainer
  let searchRequestExpirationSeconds = fromIntegral cfg.searchRequestExpirationSeconds
      driverQuoteExpirationSeconds = fromIntegral cfg.driverQuoteExpirationSeconds
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  -- FIXME: disconnect database?
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance S3AuthenticatingEntity AppEnv where
  getSecretAccessKey = (.s3Config.secretAccessKey)
  getAccessKeyId = (.s3Config.accessKeyId)
  getBucketName = (.s3Config.bucketName)
  getRegion = (.s3Config.region)

instance Registry Flow where
  registryLookup registryUrl = Registry.withSubscriberCache $ Registry.registryLookup registryUrl

cacheRegistryKey :: Text
cacheRegistryKey = "driver-offer-bpp:registry"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Cache.getKey cacheRegistryKey . lookupRequestToRedisKey
  setKey = Cache.setKey cacheRegistryKey . lookupRequestToRedisKey
  delKey = Cache.delKey cacheRegistryKey . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx cacheRegistryKey ttl . lookupRequestToRedisKey
