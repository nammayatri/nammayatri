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
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Runtime as L
import qualified EulerHS.Runtime as R
import qualified Kafka.Consumer as Consumer
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Prelude
import qualified Kernel.Storage.Beam.BecknRequest as BR
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
  startConsumerWithEnv appCfg appEnv

startConsumerWithEnv :: AppCfg -> AppEnv -> IO ()
startConsumerWithEnv appCfg appEnv@AppEnv {..} = do
  kafkaConsumer <- newKafkaConsumer
  let loggerRuntime = L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig
  R.withFlowRuntime (Just loggerRuntime) $ \flowRt' -> do
    managers <- managersFromManagersSettings appCfg.httpClientOptions.timeoutMs mempty -- default manager is created
    let flowRt = flowRt' {L._httpClientManagers = managers}
    runFlow
      flowRt
      ( prepareConnectionRider
          ( ConnectionConfigRider
              { esqDBCfg = appCfg.esqDBCfg,
                esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                hedisClusterCfg = appCfg.hedisClusterCfg
              }
          )
          appCfg.tables
      )
    CF.runConsumer flowRt appEnv consumerType kafkaConsumer
  where
    newKafkaConsumer = case appEnv.consumerType of
      RIDER_BECKN_REQUEST -> becknRequestConsumer
      DRIVER_BECKN_REQUEST -> becknRequestConsumer
      _ ->
        either (error . ("Unable to open a kafka consumer: " <>) . show) id
          <$> Consumer.newConsumer
            (kafkaConsumerCfg.consumerProperties)
            (Consumer.topics kafkaConsumerCfg.topicNames)

    becknRequestConsumer = do
      now <- getCurrentTime
      let currentTopicNumber = BR.countTopicNumber now
      let topicNumbers = filter (/= currentTopicNumber) [0 .. 23 :: Int]
      let topicNameTemplate = case kafkaConsumerCfg.topicNames of
            [tn] -> tn
            _ -> error "Should be exactly one topic name"
      let topicNames = Consumer.TopicName . ((Consumer.unTopicName topicNameTemplate <> "_") <>) . show <$> topicNumbers

      either (error . ("Unable to open a kafka consumer: " <>) . show) id
        <$> Consumer.newConsumer
          (kafkaConsumerCfg.consumerProperties)
          (Consumer.topics topicNames <> Consumer.offsetReset Consumer.Earliest)
