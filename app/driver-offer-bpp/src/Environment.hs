{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Environment where

import AWS.S3
import Beckn.External.Encryption (EncTools)
import Beckn.External.Slack.Types (SlackConfig)
import Beckn.Prelude (NominalDiffTime)
import Beckn.Sms.Config
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis as Redis
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.App
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Credentials (PrivateKey)
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Registry
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Idfy.Types.IdfyConfig as Idfy
import Storage.CachedQueries.CacheConfig
import System.Environment (lookupEnv)
import Tools.Metrics.ARDUBPPMetrics

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
    encTools :: EncTools,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    disableSignatureAuth :: Bool,
    otpSmsTemplate :: Text,
    smsCfg :: SmsConfig,
    inviteSmsTemplate :: Text,
    driverOnboardingConfigs :: DriverOnboardingConfigs,
    slackCfg :: SlackConfig,
    apiRateLimitOptions :: APIRateLimitOptions,
    driverPositionInfoExpiry :: Maybe Seconds,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    fcmTokenKeyPrefix :: Text,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    defaultRadiusOfSearch :: Meters,
    searchRequestExpirationSeconds :: Int,
    driverQuoteExpirationSeconds :: Int,
    httpClientOptions :: HttpClientOptions,
    driverUnlockDelay :: Seconds,
    idfyCfg :: Idfy.IdfyConfig,
    dashboardToken :: Text,
    defaultPickupLocThreshold :: Meters,
    defaultDropLocThreshold :: Meters,
    cacheConfig :: CacheConfig,
    metricsSearchDurationTimeout :: Seconds,
    driverPoolLimit :: Maybe Int,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverLocationUpdateNotificationTemplate :: Text
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
    driverOnboardingConfigs :: DriverOnboardingConfigs,
    slackCfg :: SlackConfig,
    apiRateLimitOptions :: APIRateLimitOptions,
    driverPositionInfoExpiry :: Maybe Seconds,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    fcmTokenKeyPrefix :: Text,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    defaultRadiusOfSearch :: Meters,
    bppMetrics :: BPPMetricsContainer,
    searchRequestExpirationSeconds :: NominalDiffTime,
    driverQuoteExpirationSeconds :: NominalDiffTime,
    driverUnlockDelay :: Seconds,
    idfyCfg :: Idfy.IdfyConfig,
    dashboardToken :: Text,
    defaultPickupLocThreshold :: Meters,
    defaultDropLocThreshold :: Meters,
    cacheConfig :: CacheConfig,
    driverPoolLimit :: Maybe Int,
    s3Env :: S3Env Flow,
    driverLocationUpdateRateLimitOptions :: APIRateLimitOptions,
    driverLocationUpdateNotificationTemplate :: Text
  }
  deriving (Generic)

data DriverOnboardingConfigs = DriverOnboardingConfigs
  { onboardingTryLimit :: Int,
    onboardingRetryTimeinHours :: Int,
    onboardSupportSmsTemplate :: Text,
    checkRCInsuranceExpiry :: Bool,
    checkRCExpiry :: Bool,
    checkRCVehicleClass :: Bool,
    checkDLExpiry :: Bool,
    checkDLVehicleClass :: Bool,
    checkImageExtraction :: Bool,
    checkImageExtractionForDashboard :: Bool
  }
  deriving (Generic, FromDhall)

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
  bppMetrics <- registerBPPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- Metrics.registerCoreMetricsContainer
  let searchRequestExpirationSeconds = fromIntegral cfg.searchRequestExpirationSeconds
      driverQuoteExpirationSeconds = fromIntegral cfg.driverQuoteExpirationSeconds
      s3Env = buildS3Env cfg.s3Config
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  -- FIXME: disconnect database?
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance Registry Flow where
  registryLookup registryUrl = Registry.withSubscriberCache $ Registry.registryLookup registryUrl

cacheRegistryKey :: Text
cacheRegistryKey = "driver-offer-bpp:registry:"

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . (cacheRegistryKey <>) . lookupRequestToRedisKey
  setKey = Redis.set . (cacheRegistryKey <>) . lookupRequestToRedisKey
  delKey = Redis.del . (cacheRegistryKey <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . (cacheRegistryKey <>) . lookupRequestToRedisKey
