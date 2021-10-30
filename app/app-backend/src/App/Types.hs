module App.Types
  ( Env,
    FlowHandler,
    FlowServer,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
  )
where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.SesConfig (SesConfig)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Flow
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified EulerHS.Types as T
import ExternalAPI.Flow
import Types.Geofencing
import Types.Metrics
import Utils.Auth

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    xGatewaySelector :: Text,
    xProviderUri :: BaseUrl,
    hostName :: Text,
    bapSelfIds :: BAPs Text,
    bapSelfURIs :: BAPs BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    searchConfirmExpiry :: Maybe Seconds,
    searchCaseExpiry :: Maybe Seconds,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    geofencingConfig :: GeofencingConfig,
    signatureExpiry :: Seconds,
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
    registrySecrets :: RegistrySecrets
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    xGatewayUri :: BaseUrl,
    xGatewaySelector :: Text,
    xProviderUri :: BaseUrl,
    hostName :: Text,
    bapSelfIds :: BAPs Text,
    bapSelfURIs :: BAPs BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    searchConfirmExpiry :: Maybe Seconds,
    searchCaseExpiry :: Maybe Seconds,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    domainVersion :: Text,
    geofencingConfig :: GeofencingConfig,
    signatureExpiry :: Seconds,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    apiRateLimitOptions :: APIRateLimitOptions,
    isShuttingDown :: TMVar (),
    bapMetrics :: BAPMetricsContainer,
    coreMetrics :: CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    registryUrl :: BaseUrl,
    registrySecrets :: RegistrySecrets
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- newEmptyTMVarIO
  bapMetrics <- registerBAPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  return $
    AppEnv
      { ..
      }

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasLookupAction LookupRegistryOrg (FlowR AppEnv) where
  runLookup = lookup
