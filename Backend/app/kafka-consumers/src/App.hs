 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
