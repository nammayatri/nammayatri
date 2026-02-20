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
import qualified Consumer.AvailabilityTime.Types as T
import qualified Consumer.BroadcastMessage.Processor as BMProcessor
import qualified Consumer.CustomerStats.Processor as PSProcessor
import qualified Consumer.LocationUpdate.Processor as LCProcessor
import Control.Error.Util
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Time as T
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (id)
import qualified Streamly.Internal.Data.Fold as SF
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Streamly.Internal.Data.Stream.Serial (SerialT)

runConsumer :: L.FlowRuntime -> AppEnv -> ConsumerType -> Consumer.KafkaConsumer -> IO ()
runConsumer flowRt appEnv consumerType kafkaConsumer = do
  case consumerType of
    AVAILABILITY_TIME -> availabilityConsumer flowRt appEnv kafkaConsumer
    BROADCAST_MESSAGE -> broadcastMessageConsumer flowRt appEnv kafkaConsumer
    PERSON_STATS -> updateCustomerStatsConsumer flowRt appEnv kafkaConsumer
    LOCATION_UPDATE -> locationUpdateConsumer flowRt appEnv kafkaConsumer

updateCustomerStatsConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
updateCustomerStatsConsumer flowRt appEnv kafkaConsumer = do
  readMesssageWithWaitAndTimeRange kafkaConsumer appEnv
    & S.chunksOf appEnv.kafkaReadBatchSize addToList
    & S.mapM updateCustomerStatsWithFlow
    & S.delay (fromIntegral $ appEnv.kafkaReadBatchDelay.getSeconds)
    & S.drain
  where
    addToList = SF.mkFold step start extract
      where
        step !acc (!val, !key, _) = SF.Partial ((val, key) : acc)
        start = SF.Partial []
        extract = reverse
    updateCustomerStatsWithFlow events = do
      runFlowR flowRt appEnv $
        generateGUID
          >>= flip withLogTag (mapM (\(eventPayload, personId) -> withLogTag ("updating-person-stats-personId:" <> personId) $ PSProcessor.updateCustomerStats eventPayload personId) events)

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
    & S.intervalsOf (fromIntegral appEnv.dumpEvery) (SF.lmap (\(message, messageKey, cr) -> ((messageKey, message.m_id), (message, cr))) (SF.classify buildTimeSeries))
    & S.mapM (Map.traverseWithKey calculateAvailableTime)
    & S.mapM (Map.traverseWithKey commitAllPartitions)
    & S.drain
  where
    commitAllPartitions _ (Just v) = traverse_ (void . Consumer.commitOffsetMessage Consumer.OffsetCommit kafkaConsumer) $ Map.elems v
    commitAllPartitions _ Nothing = pure ()

    keepMax latestCrMap cr =
      let latestCR =
            case Map.lookup (partition' cr) latestCrMap of
              Just latestCRInPartition -> if offset' cr > offset' latestCRInPartition then cr else latestCRInPartition
              Nothing -> cr
       in Map.insert (partition' cr) latestCR latestCrMap

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
        step (!acc, Nothing) (val, cr) = SF.Partial (fromMaybe val.ts val.st : acc, Just $ Map.singleton (partition' cr) cr) -- TODO: remove fromMaybe default ts once old data is processed.
        step (!acc, Just latestCrMap) (val, cr) = SF.Partial (fromMaybe val.ts val.st : acc, Just $ keepMax latestCrMap cr) -- TODO: remove fromMaybe default ts once old data is processed.
        start = SF.Partial ([], Nothing)
        extract = id

locationUpdateConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
locationUpdateConsumer flowRt appEnv kafkaConsumer = do
  let batchSize = maybe 100 (\healthCheckAppCfg -> fromIntegral healthCheckAppCfg.batchSize) appEnv.healthCheckAppCfg
  let enabledMerchantCityIds = foldMap (.enabledMerchantCityIds) appEnv.healthCheckAppCfg
  readMessages kafkaConsumer
    & S.chunksOf batchSize addToList
    & S.mapM (processRealtimeLocationUpdates' enabledMerchantCityIds)
    & S.drain
  where
    addToList ::
      MonadIO m =>
      SF.Fold
        m
        (T.LocationUpdates, T.DriverId, ConsumerRecordD)
        [(T.LocationUpdates, T.DriverId)]
    addToList = SF.mkFold step start extract
      where
        step !acc (!val, !key, _) = SF.Partial ((val, key) : acc)
        start = SF.Partial []
        extract = reverse . DL.nubBy ((==) `on` snd)
    processRealtimeLocationUpdates' enabledMerchantCityIds locationUpdate =
      runFlowR flowRt appEnv . withLogTag "pushing location batch to redis" $
        generateGUID
          >>= flip withLogTag (LCProcessor.processLocationData enabledMerchantCityIds locationUpdate)

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

readMesssageWithWaitAndTimeRange ::
  (FromJSON message, ConvertUtf8 messageKey ByteString) =>
  Consumer.KafkaConsumer ->
  AppEnv ->
  SerialT IO (message, messageKey, ConsumerRecordD)
readMesssageWithWaitAndTimeRange kafkaConsumer appEnv = do
  let eitherRecords = S.bracket (pure kafkaConsumer) Consumer.closeConsumer pollMessageR
  let records = S.mapMaybe hush eitherRecords
  S.mapMaybe (removeMaybeFromTuple . decodeRecord) records
  where
    pollMessageR kc = S.repeatM $ do
      currentHour <- liftIO getCurrentHour
      let shouldPoll = maybe True (\(start, end) -> start <= currentHour && end > currentHour) timeRange
      if shouldPoll
        then Consumer.pollMessage kc (Consumer.Timeout 500)
        else pure (Left $ Consumer.KafkaError "TIME CAME TO AN END FOR ME!!")

    timeRange = (,) <$> appEnv.consumerStartTime <*> appEnv.consumerEndTime

    getCurrentHour = do
      (T.UTCTime _date secTillNow) <- T.getCurrentTime
      return $ div (T.diffTimeToPicoseconds secTillNow) 3600000000000000

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
  LOCATION_UPDATE -> pure "location-update"
