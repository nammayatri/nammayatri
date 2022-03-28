module Environment where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    port :: Int,
    bapId :: Text,
    bapURI :: BaseUrl,
    gatewayUrl :: BaseUrl,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity',
    authServiceUrl :: BaseUrl,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    kafkaConsumerCfgs :: KafkaConsumerCfgs
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    bapId :: Text,
    bapURI :: BaseUrl,
    gatewayUrl :: BaseUrl,
    httpClientOptions :: HttpClientOptions,
    authEntity :: AuthenticatingEntity',
    authServiceUrl :: BaseUrl,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer,
    kafkaConsumerEnv :: KafkaConsumerEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  kafkaConsumerEnv <- buildKafkaConsumerEnv kafkaConsumerCfgs
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  releaseKafkaConsumerEnv kafkaConsumerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)
