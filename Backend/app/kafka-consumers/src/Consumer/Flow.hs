{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE BangPatterns #-}

module Consumer.Flow where

import qualified Consumer.AvailabilityTime.Processor as ATProcessor
import qualified Consumer.BecknRequest.Processor as BRProcessor
import qualified Consumer.BroadcastMessage.Processor as BMProcessor
import qualified Consumer.CustomerStats.Processor as PSProcessor
import Control.Error.Util hiding (err)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import qualified Data.HashMap as HM
import qualified Data.Map.Strict as Map
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import qualified Kernel.Storage.Beam.BecknRequest as BR
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.Common (generateGUID, getCurrentTime, withLogTag)
import qualified Streamly.Internal.Data.Fold as SF
import Streamly.Internal.Data.Stream.Serial (SerialT)
import qualified Streamly.Prelude as S

runConsumer :: L.FlowRuntime -> AppEnv -> ConsumerType -> Consumer.KafkaConsumer -> IO ()
runConsumer flowRt appEnv consumerType kafkaConsumer = do
  case consumerType of
    AVAILABILITY_TIME -> availabilityConsumer flowRt appEnv kafkaConsumer
    BROADCAST_MESSAGE -> broadcastMessageConsumer flowRt appEnv kafkaConsumer
    PERSON_STATS -> updateCustomerStatsConsumer flowRt appEnv kafkaConsumer
    RIDER_BECKN_REQUEST -> becknRequestConsumer BRProcessor.RIDER flowRt appEnv kafkaConsumer
    DRIVER_BECKN_REQUEST -> becknRequestConsumer BRProcessor.DRIVER flowRt appEnv kafkaConsumer

updateCustomerStatsConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
updateCustomerStatsConsumer flowRt appEnv kafkaConsumer =
  readMessages kafkaConsumer
    & S.mapM updateCustomerStatsWithFlow
    & S.drain
  where
    updateCustomerStatsWithFlow (messagePayload, personId, _) =
      runFlowR flowRt appEnv . withLogTag personId $
        generateGUID
          >>= flip withLogTag (PSProcessor.updateCustomerStats messagePayload personId)

broadcastMessageConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
broadcastMessageConsumer flowRt appEnv kafkaConsumer =
  readMessages kafkaConsumer
    & S.mapM broadcastMessageWithFlow
    & S.drain
  where
    broadcastMessageWithFlow (messagePayload, driverId, _) =
      runFlowR flowRt appEnv . withLogTag driverId $
        generateGUID
          >>= flip withLogTag (BMProcessor.broadcastMessage messagePayload driverId)

availabilityConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
availabilityConsumer flowRt appEnv kafkaConsumer =
  readMessages kafkaConsumer
    & S.mapM (\(message, messageKey, cr) -> processRealtimeLocationUpdates message messageKey $> (message, messageKey, cr))
    & S.intervalsOf (fromIntegral appEnv.dumpEvery) (SF.lmap (\(message, messageKey, cr) -> ((messageKey, message.mId), (message, cr))) (SF.classify buildTimeSeries))
    & S.mapM (Map.traverseWithKey calculateAvailableTime)
    & S.mapM (Map.traverseWithKey commitAllPartitions)
    & S.drain
  where
    commitAllPartitions _ (Just v) = traverse_ (void . Consumer.commitOffsetMessage Consumer.OffsetCommit kafkaConsumer) $ HM.elems v
    commitAllPartitions _ Nothing = pure ()

    keepMax latestCrMap cr =
      let latestCR =
            case HM.lookup (partition' cr) latestCrMap of
              Just latestCRInPartition -> if offset' cr > offset' latestCRInPartition then cr else latestCRInPartition
              Nothing -> cr
       in HM.insert (partition' cr) latestCR latestCrMap

    calculateAvailableTime (driverId, merchantId) (timeSeries, mbCR) = do
      mbCR
        <$ ( runFlowR flowRt appEnv . withLogTag driverId $
               generateGUID
                 >>= flip withLogTag (ATProcessor.calculateAvailableTime merchantId driverId (reverse timeSeries))
           )

    processRealtimeLocationUpdates locationUpdate driverId =
      runFlowR flowRt appEnv . withLogTag driverId $
        generateGUID
          >>= flip withLogTag (ATProcessor.processData locationUpdate driverId)

    offset' = Consumer.unOffset . Consumer.crOffset
    partition' = Consumer.unPartitionId . Consumer.crPartition

    -- buildTimeSeries :: MonadIO m =>
    --   SF.Fold
    --   m
    --   (LocationUpdates, ConsumerRecordD)
    --   ([UTCTime], Maybe ConsumerRecordD)
    buildTimeSeries = SF.mkFold step start extract
      where
        step (!acc, Nothing) (val, cr) = SF.Partial (fromMaybe val.ts val.st : acc, Just $ HM.singleton (partition' cr) cr) -- TODO: remove fromMaybe default ts once old data is processed.
        step (!acc, Just latestCrMap) (val, cr) = SF.Partial (fromMaybe val.ts val.st : acc, Just $ keepMax latestCrMap cr) -- TODO: remove fromMaybe default ts once old data is processed.
        start = SF.Partial ([], Nothing)
        extract = id

becknRequestConsumer :: BRProcessor.BecknRequestType -> L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
becknRequestConsumer becknRequestType flowRt appEnv kafkaConsumer = do
  flip finally (Consumer.closeConsumer kafkaConsumer) $ do
    readBecknRequestMessages kafkaConsumer
      >>= riderDrainerWithFlow
      >>= commitAllTopics
  where
    riderDrainerWithFlow messages = do
      runFlowR flowRt appEnv $
        generateGUID
          >>= flip withLogTag (BRProcessor.becknRequestProcessor becknRequestType messages)
      pure messages

    commitAllTopics messages = case messages of
      [] -> print ("Nothing to commit" :: Text)
      _ -> do
        mbErr <- Consumer.commitAllOffsets Consumer.OffsetCommit kafkaConsumer
        whenJust mbErr $ \err -> print ("Error while commit: message: " <> show err :: Text)

readBecknRequestMessages ::
  Consumer.KafkaConsumer ->
  IO [BR.BecknRequestKafka]
readBecknRequestMessages kafkaConsumer = do
  pollStartTime <- getCurrentTime
  let currentTopic = BR.countTopicNumber pollStartTime
  eitherRecords <- pollMessageLoop (currentTopic, 0, [])
  let records = mapMaybe hush eitherRecords
  mbDecodedRecords <- forM records $ \record -> do
    case Consumer.crValue record of
      Just value -> do
        case A.eitherDecode @BR.BecknRequestKafka . LBS.fromStrict $ value of
          Right v -> pure (Just v)
          Left err -> throwIO (InternalError $ "Could not decode record: " <> show value <> "; message: " <> show err) -- only when beckn_request tabular type was changed
      Nothing -> pure Nothing
  pure $ catMaybes mbDecodedRecords
  where
    pollMessageLoop :: (Int, Int, [Either Consumer.KafkaError ConsumerRecordD]) -> IO [Either Consumer.KafkaError ConsumerRecordD]
    pollMessageLoop (currentTopic, emptyMessagesCount, messages) = do
      eMessage <- Consumer.pollMessage kafkaConsumer (Consumer.Timeout 500)
      (continueLoop, updEmptyMessagesCount) <- predicate currentTopic emptyMessagesCount eMessage
      let updMessages = eMessage : messages
      if continueLoop then pollMessageLoop (currentTopic, updEmptyMessagesCount, updMessages) else pure updMessages

    predicate currentTopic emptyMessagesCount message = do
      now <- getCurrentTime
      if BR.countTopicNumber now /= currentTopic
        then do
          print ("Close consumer: current partition changed, not all messages was read!" :: Text)
          pure (False, emptyMessagesCount)
        else do
          let isEmptyMessage = either (const True) (isNothing . Consumer.crValue) message
          if isEmptyMessage
            then do
              let newCount = emptyMessagesCount + 1
              unless (newCount < 10) $
                print ("Close consumer: received 10 empty messages" :: Text)
              pure (newCount < 10, newCount)
            else do
              pure (True, 0)

readMessages ::
  (FromJSON message, ConvertUtf8 messageKey ByteString) =>
  Consumer.KafkaConsumer ->
  SerialT IO (message, messageKey, ConsumerRecordD)
readMessages kafkaConsumer = do
  let eitherRecords = S.bracket (pure kafkaConsumer) Consumer.closeConsumer pollMessageR
  let records = S.mapMaybe hush eitherRecords
  S.mapMaybe (removeMaybeFromTuple . decodeRecord) records
  where
    pollMessageR kc = S.repeatM (Consumer.pollMessage kc (Consumer.Timeout 500))

    -- convert ConsumerRecord into domain types of (Message, (MessageKey, ConsumerRecord))
    decodeRecord =
      (A.decode . LBS.fromStrict <=< Consumer.crValue)
        &&& (pure . decodeUtf8 <=< Consumer.crKey)
        &&& id

    -- remove maybe messages and convert to tuple
    removeMaybeFromTuple (mbMessage, (mbMessageKey, cr)) =
      (\message messageKey -> (message, messageKey, cr)) <$> mbMessage <*> mbMessageKey

getConfigNameFromConsumerType :: ConsumerType -> IO String
getConfigNameFromConsumerType = \case
  AVAILABILITY_TIME -> pure "driver-availability-calculator"
  BROADCAST_MESSAGE -> pure "broadcast-message"
  PERSON_STATS -> pure "person-stats"
  RIDER_BECKN_REQUEST -> pure "rider-beckn-request-consumer"
  DRIVER_BECKN_REQUEST -> pure "driver-beckn-request-consumer"
