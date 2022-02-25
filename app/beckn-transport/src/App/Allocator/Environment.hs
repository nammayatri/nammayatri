module App.Allocator.Environment where

import App.Allocator.Config
import Beckn.External.Encryption (EncTools)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth
import Beckn.Utils.Shutdown
import EulerHS.Prelude
import Tools.Streaming.Kafka.Environment
import Types.Metrics

type Flow = FlowR AppEnv

data AppEnv = AppEnv
  { config :: AppCfg,
    dbCfg :: DBConfig,
    encTools :: EncTools,
    fcmJsonPath :: Maybe Text,
    fcmUrl :: BaseUrl,
    nwAddress :: BaseUrl,
    driverPositionInfoExpiry :: Maybe Seconds,
    defaultRadiusOfSearch :: Meters,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    btmMetrics :: AllocatorMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  isShuttingDown <- mkShutdown
  btmMetrics <- registerAllocatorMetricsContainer
  hostname <- getPodName
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  pure AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.config.appCfg.signingKey)
  getSignatureExpiry = (.config.appCfg.signatureExpiry)
