module App.Types where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Redis.Config
import Beckn.Tools.Metrics.Types
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Types.Registry
import Beckn.Utils.App (getPodName)
import qualified Beckn.Utils.CacheRedis as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import Tools.Streaming.Kafka.Environment

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    redisCfg :: RedisConfig,
    port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    metricsSearchDurationTimeout :: Seconds,
    gatewayUrl :: BaseUrl,
    selfId :: Text,
    selfURI :: BaseUrl,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity',
    coreVersion :: Text,
    registryUrl :: BaseUrl,
    domainVersion :: Text,
    authServiceUrl :: BaseUrl,
    disableSignatureAuth :: Bool,
    hostName :: Text,
    kafkaProducerCfg :: KafkaProducerCfg
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { config :: AppCfg,
    redisCfg :: RedisConfig,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer,
    bapMetrics :: BAPMetricsContainer,
    coreVersion :: Text,
    domainVersion :: Text,
    kafkaProducerTools :: KafkaProducerTools,
    kafkaEnvs :: BAPKafkaEnvs
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  coreMetrics <- registerCoreMetricsContainer
  bapMetrics <- registerBAPMetricsContainer metricsSearchDurationTimeout
  isShuttingDown <- mkShutdown
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  kafkaEnvs <- buildBAPKafkaEnvs
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.config.authEntity.signingKey)
  getSignatureExpiry = (.config.authEntity.signatureExpiry)

instance Registry Flow where
  registryLookup = Registry.withSubscriberCache Registry.registryLookup

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Cache.getKey "registry" . lookupRequestToRedisKey
  setKey = Cache.setKey "registry" . lookupRequestToRedisKey
  delKey = Cache.delKey "registry" . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx "registry" ttl . lookupRequestToRedisKey
