module App (startKafkaConsumer) where

import qualified Consumer.Flow as CF
import Data.Function
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import System.Environment (lookupEnv)

startKafkaConsumer :: IO ()
startKafkaConsumer = do
  consumerType :: ConsumerType <- read . fromMaybe "AVAILABILITY_TIME" <$> lookupEnv "CONSUMER_TYPE"
  configFile <- CF.getConfigNameFromConsumertype consumerType
  appCfg :: AppCfg <- readDhallConfigDefault configFile
  appEnv <- buildAppEnv appCfg consumerType
  flowRt' <- L.createFlowRuntime' (Just $ L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig)
  managers <- managersFromManagersSettings appCfg.httpClientOptions.timeoutMs mempty -- default manager is created
  let flowRt = flowRt' {L._httpClientManagers = managers}
  startConsumerWithEnv flowRt appEnv

startConsumerWithEnv :: L.FlowRuntime -> AppEnv -> IO ()
startConsumerWithEnv flowRt appEnv@AppEnv {..} = do
  kafkaConsumer <- newKafkaConsumer
  CF.runConsumer flowRt appEnv consumerType kafkaConsumer
  where
    newKafkaConsumer =
      either (error . ("Unable to open a kafka consumer: " <>) . show) id
        <$> Consumer.newConsumer
          (kafkaConsumerCfg.consumerProperties)
          (Consumer.topics kafkaConsumerCfg.topicNames)
