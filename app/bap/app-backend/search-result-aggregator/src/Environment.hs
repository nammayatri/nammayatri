module Environment where

import Kernel.Prelude
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, connectHedis)
import Kernel.Storage.Hedis.AppPrefixes (riderAppPrefix)
import Kernel.Types.Common
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    kafkaConsumerCfgs :: KafkaConsumerCfgs,
    hedisCfg :: HedisCfg
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer,
    kafkaConsumerEnv :: KafkaConsumerEnv,
    hedisEnv :: HedisEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  kafkaConsumerEnv <- buildKafkaConsumerEnv kafkaConsumerCfgs
  hedisEnv <- connectHedis hedisCfg riderAppPrefix
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  releaseKafkaConsumerEnv kafkaConsumerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
