 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE BangPatterns #-}

module App (startKafkaConsumer) where

import Control.Error.Util
import Control.Exception (ErrorCall (ErrorCall))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import qualified DynamicOfferDriverApp.Processor as DO
import Environment
import qualified EulerHS.Runtime as L
import Kafka.Consumer (ConsumerRecord (crKey, crValue))
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Flow
import Kernel.Utils.Common (generateGUID, throwError, withLogTag)
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import qualified Streamly.Internal.Data.Fold as SF
import qualified Streamly.Internal.Prelude as S
import System.Environment (lookupEnv)

getConfigNameFromConsumertype :: ConsumerType -> IO String
getConfigNameFromConsumertype = \case
  AVAILABILITY_TIME -> pure "driver-availability-calculator"
  FEED_TO_CLICKHOUSE -> throwIO $ ErrorCall "Not Specified yet."

startKafkaConsumer :: IO ()
startKafkaConsumer = do
  consumerType :: ConsumerType <- read . fromMaybe "AVAILABILITY_TIME" <$> lookupEnv "CONSUMER_TYPE"
  configFile <- getConfigNameFromConsumertype consumerType
  appCfg :: AppCfg <- readDhallConfigDefault configFile
  appEnv <- buildAppEnv appCfg consumerType
  flowRt <- L.createFlowRuntime' (Just $ L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig)
  startConsumerWithEnv flowRt appEnv

startConsumerWithEnv :: L.FlowRuntime -> AppEnv -> IO ()
startConsumerWithEnv flowRt appEnv@AppEnv {..} = do
  kafkaConsumer <- newKafkaConsumer
  processData kafkaConsumer
  where
    newKafkaConsumer =
      either (error . ("Unable to open a kafka consumer: " <>) . show) id
        <$> Consumer.newConsumer
          (kafkaConsumerCfg.consumerProperties)
          (Consumer.topics kafkaConsumerCfg.topicNames)

    processData kafkaConsumer = do
      case consumerType of
        AVAILABILITY_TIME -> processStreamforAvailability kafkaConsumer
        FEED_TO_CLICKHOUSE -> clickhousefeeder

    clickhousefeeder = withFlow . throwError $ InternalError "Implementation missing"

    processStreamforAvailability kafkaConsumer =
      readMessages kafkaConsumer
        & S.mapMaybe hush
        & S.mapMaybe ((\(mbMessage, (mbMessageKey, cr)) -> (\message messageKey -> (message, messageKey, cr)) <$> mbMessage <*> mbMessageKey) . ((A.decode . LBS.fromStrict <=< crValue) &&& (pure . decodeUtf8 <=< crKey) &&& id))
        & S.mapM (\(message, messageKey, cr) -> processRealtimeLocationUpdates message messageKey $> (message, messageKey, cr))
        & S.intervalsOf (fromIntegral dumpEvery) (SF.lmap (\(message, messageKey, cr) -> ((messageKey, message.mId), (message, cr))) (SF.classify buildTimeSeries))
        & S.mapM (calculateAvailableTime kafkaConsumer)
        & S.drain

    calculateAvailableTime kafkaConsumer driverLocationUpdatesMap = withFlow $ generateGUID >>= flip withLogTag (DO.calculateAvailableTime kafkaConsumer driverLocationUpdatesMap)

    processRealtimeLocationUpdates locationUpdate driverId = withFlow . withLogTag driverId $ generateGUID >>= flip withLogTag (DO.processData locationUpdate driverId)

    buildTimeSeries = SF.mkFold step start extract
      where
        step (!acc, _) (val, cr) = pure (val.ts : acc, Just cr)
        start = pure ([], Nothing)
        extract = pure

    readMessages kafkaConsumer =
      S.bracket
        (pure kafkaConsumer)
        Consumer.closeConsumer
        (\kc -> S.repeatM (Consumer.pollMessage kc (Consumer.Timeout 500)))

    withFlow fn = runFlowR flowRt appEnv fn
