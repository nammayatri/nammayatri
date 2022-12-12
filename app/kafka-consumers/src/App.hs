{-# LANGUAGE BangPatterns #-}

module App (startKafkaConsumer) where

import Beckn.Prelude
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Flow
import Beckn.Utils.Common (throwError)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.FlowLogging as L
import Control.Error.Util
import Control.Exception (ErrorCall (ErrorCall))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import qualified Data.Map.Strict as Map
import qualified DriverOfferBPP.Processor as DO
import Environment
import EulerHS.Runtime (withFlowRuntime)
import Kafka.Consumer (ConsumerRecord (crKey, crValue))
import qualified Kafka.Consumer as Consumer
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
  startConsumerWithEnv appEnv

startConsumerWithEnv :: AppEnv -> IO ()
startConsumerWithEnv appEnv@AppEnv {..} = do
  processData readMessages
  where
    newKafkaConsumer =
      either (error . ("Unable to open a kafka consumer: " <>) . show) id
        <$> Consumer.newConsumer
          (kafkaConsumerCfg.consumerProperties)
          (Consumer.topics kafkaConsumerCfg.topicNames)

    processData msges =
      case consumerType of
        AVAILABILITY_TIME -> processStreamforAvailability msges
        FEED_TO_CLICKHOUSE -> clickhousefeeder

    clickhousefeeder = withFlow . throwError $ InternalError "Implementation missing"

    processStreamforAvailability msges =
      msges
        & S.mapMaybe hush
        & S.mapMaybe ((\(a, b) -> (,) <$> a <*> b) . ((A.decode . LBS.fromStrict <=< crValue) &&& (pure . decodeUtf8 <=< crKey)))
        & S.mapM (\(a, b :: Text) -> processRealtimeLocationUpdates a b $> (a, b))
        & S.intervalsOf (fromIntegral dumpEvery) (SF.lmap (\(a, b) -> ((b, a.mId), a)) (SF.classify buildTimeSeries))
        & S.mapM (Map.traverseWithKey calculateAvailableTime)
        & S.drain

    calculateAvailableTime (driverId, merchantId) = withFlow . DO.calculateAvailableTime merchantId driverId . reverse

    processRealtimeLocationUpdates locationUpdate = withFlow . DO.processData locationUpdate

    buildTimeSeries = SF.mkFold step start extract
      where
        step !acc val = pure (val.ts : acc)
        start = pure []
        extract = pure

    readMessages =
      S.bracket
        newKafkaConsumer
        Consumer.closeConsumer
        (\kc -> S.repeatM (Consumer.pollMessage kc (Consumer.Timeout 500)))

    withFlow fn = do
      withFlowRuntime
        (Just $ L.getEulerLoggerRuntime hostname loggerConfig)
        \flowRt -> runFlowR flowRt appEnv fn
