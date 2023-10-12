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
import qualified Consumer.BroadcastMessage.Processor as BMProcessor
import qualified Consumer.CustomerStats.Processor as PSProcessor
import qualified Consumer.KafkaTable.Processor as KTProcessor
import Control.Error.Util hiding (err)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import qualified Data.HashMap as HM
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import qualified Kernel.Streaming.Kafka.KafkaTable as Kafka
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.Common (generateGUID, getCurrentTime, withLogTag)
import qualified Streamly.Internal.Data.Fold as SF
import Streamly.Internal.Data.Stream.Serial (SerialT)
import qualified Streamly.Internal.Prelude as S

runConsumer :: L.FlowRuntime -> AppEnv -> ConsumerType -> Consumer.KafkaConsumer -> IO ()
runConsumer flowRt appEnv consumerType kafkaConsumer = do
  case consumerType of
    AVAILABILITY_TIME -> availabilityConsumer flowRt appEnv kafkaConsumer
    BROADCAST_MESSAGE -> broadcastMessageConsumer flowRt appEnv kafkaConsumer
    PERSON_STATS -> updateCustomerStatsConsumer flowRt appEnv kafkaConsumer
    KAFKA_TABLE -> kafkaTableConsumer flowRt appEnv kafkaConsumer

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

    buildTimeSeries = SF.mkFold step start extract
      where
        step (!acc, Nothing) (val, cr) = pure (fromMaybe val.ts val.st : acc, Just $ HM.singleton (partition' cr) cr) -- TODO: remove fromMaybe default ts once old data is processed.
        step (!acc, Just latestCrMap) (val, cr) = pure (fromMaybe val.ts val.st : acc, Just $ keepMax latestCrMap cr) -- TODO: remove fromMaybe default ts once old data is processed.
        start = pure ([], Nothing)
        extract = pure

kafkaTableConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
kafkaTableConsumer flowRt appEnv kafkaConsumer = do
  flip finally (Consumer.closeConsumer kafkaConsumer) $ do
    readKafkaTableMessages kafkaConsumer
      & S.foldr foldFunc Map.empty
      >>= riderDrainerWithFlow
      >>= commitAllTopics
  where
    riderDrainerWithFlow messages = do
      runFlowR flowRt appEnv $
        generateGUID
          >>= flip withLogTag (KTProcessor.kafkaTableProcessor messages)
      pure messages

    commitAllTopics messages =
      if Map.null messages
        then print ("Nothing to commit" :: Text)
        else do
          mbErr <- Consumer.commitAllOffsets Consumer.OffsetCommit kafkaConsumer
          whenJust mbErr $ \err -> print ("Error while commit: message: " <> show err :: Text)

    foldFunc ::
      Kafka.KafkaTable ->
      Map.Map String [Kafka.KafkaTable] ->
      Map.Map String [Kafka.KafkaTable]
    foldFunc kafkaTable mapKafkaTable = do
      let filePath = mkFilePathWithoutPrefix kafkaTable
      Map.insertWithKey (\_key newList oldList -> newList <> oldList) filePath [kafkaTable] mapKafkaTable

    mkFilePathWithoutPrefix :: Kafka.KafkaTable -> String
    mkFilePathWithoutPrefix kafkaTable =
      do
        T.unpack kafkaTable.schemaName
        <> "/"
        <> T.unpack kafkaTable.tableName
        <> "/"
        <> Time.formatTime Time.defaultTimeLocale "%Y.%m.%d-%H" kafkaTable.timestamp
        <> ".json"

readKafkaTableMessages ::
  Consumer.KafkaConsumer ->
  SerialT IO Kafka.KafkaTable
readKafkaTableMessages kafkaConsumer = do
  let eitherRecords = S.bracket before after pollMessageR
  let records = S.mapMaybe hush eitherRecords
  flip S.mapMaybeM records $ \record -> do
    case Consumer.crValue record of
      Just value -> do
        case A.eitherDecode @Kafka.KafkaTable . LBS.fromStrict $ value of
          Right v -> pure (Just v)
          Left err -> throwIO (InternalError $ "Could not decode record: " <> show value <> "; message: " <> show err) -- only when KafkaTable type changed
      Nothing -> pure Nothing
  where
    before = do
      pollStartTime <- getCurrentTime
      emptyMessagesCount <- newIORef (0 :: Int)
      let currentTopic = Kafka.countTopicNumber pollStartTime
      pure (currentTopic, emptyMessagesCount)

    after (_currentTopic, _emptyMessagesCount) = do
      pure () -- consumer will be closed after messages handled and commited
    predicate currentTopic emptyMessagesCount message = do
      now <- getCurrentTime
      if Kafka.countTopicNumber now /= currentTopic
        then do
          print ("Close consumer: current partition changed, not all messages was read!" :: Text)
          pure False
        else do
          let isEmptyMessage = either (const True) (isNothing . Consumer.crValue) message
          if isEmptyMessage
            then do
              newCount <- atomicModifyIORef emptyMessagesCount (\count -> (count + 1, count + 1))
              unless (newCount < 10) $
                print ("Close consumer: received 10 empty messages" :: Text)
              pure $ newCount < 10
            else do
              atomicModifyIORef emptyMessagesCount (const (0, True))

    pollMessageR (currentTopic, emptyMessagesCount) =
      S.takeWhileM (predicate currentTopic emptyMessagesCount) $
        S.repeatM $ Consumer.pollMessage kafkaConsumer (Consumer.Timeout 500)

readMessages ::
  (FromJSON a, ConvertUtf8 aKey ByteString) =>
  Consumer.KafkaConsumer ->
  SerialT IO (a, aKey, ConsumerRecordD)
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
  KAFKA_TABLE -> pure "kafka-table-consumer"
