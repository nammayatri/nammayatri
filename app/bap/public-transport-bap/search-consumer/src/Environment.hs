module Environment where

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Common
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Servant.SignatureAuth
import Kernel.Utils.Shutdown
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
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authEntity :: AuthenticatingEntity',
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
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authEntity :: AuthenticatingEntity',
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
