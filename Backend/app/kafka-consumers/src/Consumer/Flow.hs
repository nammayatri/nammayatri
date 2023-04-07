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
import Control.Error.Util
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
import Kernel.Types.Flow
import Kernel.Utils.Common (generateGUID, withLogTag)
import qualified Streamly.Internal.Data.Fold as SF
import Streamly.Internal.Data.Stream.Serial (SerialT)
import qualified Streamly.Prelude as S

import Consumer.AvailabilityTime.Types (LocationUpdates)

runConsumer :: L.FlowRuntime -> AppEnv -> ConsumerType -> Consumer.KafkaConsumer -> IO ()
runConsumer flowRt appEnv consumerType kafkaConsumer = do
  case consumerType of
    AVAILABILITY_TIME -> availabilityConsumer flowRt appEnv kafkaConsumer
    BROADCAST_MESSAGE -> broadcastMessageConsumer flowRt appEnv kafkaConsumer
    PERSON_STATS -> updateCustomerStatsConsumer flowRt appEnv kafkaConsumer

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

getConfigNameFromConsumertype :: ConsumerType -> IO String
getConfigNameFromConsumertype = \case
  AVAILABILITY_TIME -> pure "driver-availability-calculator"
  BROADCAST_MESSAGE -> pure "broadcast-message"
  PERSON_STATS -> pure "person-stats"
