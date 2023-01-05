module DriverOfferBPP.Processor
  ( processData,
    calculateAvailableTime,
  )
where

import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Hedis as Redis
import qualified Beckn.Types.Common as C
import Beckn.Types.Flow (FlowR)
import qualified Beckn.Types.SlidingWindowCounters as SWT
import Beckn.Utils.Logging (logInfo)
import qualified Beckn.Utils.SlidingWindowCounters as SW
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified DriverOfferBPP.Storage.Queries as Q
import qualified DriverOfferBPP.Types as T
import Environment (AppEnv)
import EulerHS.Prelude
import qualified Kafka.Consumer as Consumer

type Flow = FlowR AppEnv

getTimeDiffInteger :: SWT.TimePair -> Integer
getTimeDiffInteger (startTime, endTime) = floor $ diffUTCTime endTime startTime

createOrUpdateDriverAvailability :: Text -> Text -> SWT.TimePair -> (Integer, UTCTime) -> Flow ()
createOrUpdateDriverAvailability merchantId driverId (bucketStartTime, bucketEndTime) (valToAdd, lastAvailableTime) = do
  daId <- C.generateGUID
  currTime <- liftIO getCurrentTime
  let newAvailabilityEntry =
        T.DriverAvailability
          { id = daId,
            createdAt = currTime,
            updatedAt = currTime,
            totalAvailableTime = fromInteger valToAdd,
            ..
          }
  DB.runTransaction $ Q.createOrUpdateDriverAvailability newAvailabilityEntry

calculateAvailableTime :: Text -> Text -> Consumer.KafkaConsumer -> ([UTCTime], Maybe (Consumer.ConsumerRecord (Maybe ByteString) (Maybe ByteString))) -> Flow ()
calculateAvailableTime _ _ _ ([], Nothing) = pure ()
calculateAvailableTime _ _ _ ([], Just _) = logInfo "Should never reach here, no locationupdates but kafka consumer record :-/ "
calculateAvailableTime _ _ _ (_, Nothing) = logInfo "Should never reach here, locationupdates but no kafka consumer record :-/"
calculateAvailableTime merchantId driverId kc (fstTime : restTimeSeries, Just lastCR) = do
  mbLatestAvailabilityRecord <- Q.findLatestByDriverIdAndMerchantId driverId merchantId
  timeBetweenUpdates <- asks (.timeBetweenUpdates)
  granualityPeriodType <- asks (.granualityPeriodType)
  let lastAvailableTime = fromMaybe fstTime $ mbLatestAvailabilityRecord <&> (.lastAvailableTime)
      activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastAvailableTime granualityPeriodType $ filter (> lastAvailableTime) (fstTime : restTimeSeries)
      availableTime = sumPairDiff activeTimePairs
      availabilityInWindow =
        foldr
          ( \timePair acc -> do
              let bucketPair = getBucketPair granualityPeriodType timePair
              flip (M.insert bucketPair) acc $
                case M.lookup bucketPair acc of
                  Just (oldAvailabileTime, _) -> (oldAvailabileTime + getTimeDiffInteger timePair, snd timePair)
                  Nothing -> (getTimeDiffInteger timePair, snd timePair)
          )
          M.empty
          activeTimePairs
  logInfo $ "ActiveTime pairs " <> show availabilityInWindow
  logInfo $ "Adding " <> show availableTime <> " seconds available time for driverId: " <> show driverId
  void $ M.traverseWithKey (createOrUpdateDriverAvailability merchantId driverId) availabilityInWindow
  void $ Consumer.commitOffsetMessage Consumer.OffsetCommit kc lastCR
  where
    sumPairDiff = foldr' (\timePair acc -> acc + uncurry (flip diffUTCTime) timePair) 0
    getBucketPair periodType (_, endTime) = do
      let bucketEndTime = SW.incrementPeriod periodType endTime
      let bucketStartTime = flip addUTCTime bucketEndTime . fromInteger $ -1 * SW.convertPeriodTypeToSeconds periodType
      (bucketStartTime, bucketEndTime)

mkPairsWithLessThenThreshold :: Integer -> UTCTime -> SWT.PeriodType -> [UTCTime] -> [SWT.TimePair]
mkPairsWithLessThenThreshold timeBetweenUpdates lastAvailableTime granualityPeriodType =
  snd
    . foldl
      ( \(oldTime, newList) newTime ->
          if (floor (diffUTCTime newTime oldTime) :: Integer) <= timeBetweenUpdates
            then (newTime, newList <> SW.splitOnPeriodGranuality granualityPeriodType (oldTime, newTime))
            else (newTime, newList)
      )
      (lastAvailableTime, [])

processData :: T.LocationUpdates -> Text -> Flow ()
processData T.LocationUpdates {..} driverId = do
  let newUpdatedAt = ts
  timeBetweenUpdates <- asks (.timeBetweenUpdates)
  windowOptions <- asks (.windowOptions)
  lastUpdatedAt <- fromMaybe newUpdatedAt <$> Redis.get (mkLastTimeStampKey driverId)
  unless (lastUpdatedAt > newUpdatedAt) $ do
    Redis.setExp (mkLastTimeStampKey driverId) newUpdatedAt 14400 -- 4 hours
    let activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastUpdatedAt windowOptions.periodType [lastUpdatedAt, newUpdatedAt]
    mapM_
      ( \(startTime, endTime) -> do
          let valueToAdd = getTimeDiffInteger (startTime, endTime)
          SW.incrementByValueInTimeBucket startTime valueToAdd (mkAvailableTimeKey driverId) windowOptions
      )
      activeTimePairs
  where
    mkAvailableTimeKey :: Text -> Text
    mkAvailableTimeKey = (<> (mId <> "-available-time"))

    mkLastTimeStampKey :: Text -> Text
    mkLastTimeStampKey = (<> (mId <> "-last-location-update-at"))
