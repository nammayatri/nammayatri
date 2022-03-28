module App.Types where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Redis.Config (RedisConfig)
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Types.Registry
import Beckn.Utils.App (getPodName)
import qualified Beckn.Utils.CacheRedis as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client (HttpClientOptions (..))
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import qualified Tools.Metrics as Metrics

data AppCfg = AppCfg
  { port :: Int,
    esqDBCfg :: EsqDBConfig,
    redisCfg :: RedisConfig,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    selfId :: Text,
    selfURI :: BaseUrl,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity',
    authServiceUrl :: BaseUrl,
    gatewayUrl :: BaseUrl,
    metricsSearchDurationTimeout :: Seconds,
    coreVersion :: Text,
    domainVersion :: Text,
    hostName :: Text,
    registryUrl :: BaseUrl,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    disableSignatureAuth :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    redisCfg :: RedisConfig,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    selfId :: Text,
    selfURI :: BaseUrl,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity',
    authServiceUrl :: BaseUrl,
    gatewayUrl :: BaseUrl,
    coreVersion :: Text,
    domainVersion :: Text,
    hostName :: Text,
    registryUrl :: BaseUrl,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    disableSignatureAuth :: Bool,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: Metrics.CoreMetricsContainer,
    bapMetrics :: Metrics.BAPMetricsContainer
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  coreMetrics <- Metrics.registerCoreMetricsContainer
  bapMetrics <- Metrics.registerBAPMetricsContainer metricsSearchDurationTimeout
  isShuttingDown <- mkShutdown
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)

instance Registry Flow where
  registryLookup = Registry.withSubscriberCache Registry.registryLookup

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Cache.getKey "registry" . lookupRequestToRedisKey
  setKey = Cache.setKey "registry" . lookupRequestToRedisKey
  delKey = Cache.delKey "registry" . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx "registry" ttl . lookupRequestToRedisKey
