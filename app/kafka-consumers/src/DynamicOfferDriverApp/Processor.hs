module DynamicOfferDriverApp.Processor
  ( processData,
    calculateAvailableTime,
  )
where

import qualified Data.HashMap as HM
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified DynamicOfferDriverApp.Storage.Queries as Q
import qualified DynamicOfferDriverApp.Types as T
import Environment (AppEnv)
import EulerHS.Prelude
import qualified Kafka.Consumer as C
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Common as C hiding (Offset)
import Kernel.Types.Flow (FlowR)
import qualified Kernel.Types.SlidingWindowCounters as SWT
import Kernel.Utils.Common (withLogTag)
import Kernel.Utils.Logging (logInfo)
import qualified Kernel.Utils.SlidingWindowCounters as SW

type Flow = FlowR AppEnv

getTimeDiffInteger :: SWT.TimePair -> Integer
getTimeDiffInteger (startTime, endTime) = floor $ diffUTCTime endTime startTime

calculateAvailableTime :: C.KafkaConsumer -> Map (T.MerchantId, T.DriverId) ([UTCTime], Maybe T.ConsumerRecordD) -> Flow ()
calculateAvailableTime kafkaConsumer driverLocationUpdatesMap = do
  newDriverAvaiabilityBuckets <- M.traverseWithKey calculateAvailableTime' driverLocationUpdatesMap
  bucketAlreadyInDb <- Q.getAvailabilityInBucket newDriverAvaiabilityBuckets
  udpatedDriverAvailabilityBuckets <- concatMapM (createUpdatedTimeBuckets (Q.makeMapWithUniqueFields bucketAlreadyInDb)) (M.toList newDriverAvaiabilityBuckets)
  let crsToCommit = getLatestCRInEachPartition
  DB.runTransaction $ Q.insertIntoDB bucketAlreadyInDb udpatedDriverAvailabilityBuckets
  mapM_ (C.commitOffsetMessage C.OffsetCommit kafkaConsumer) crsToCommit
  where
    getLatestCRInEachPartition =
      map snd
        . HM.elems
        . foldl'
          ( \patitionOffsetMap cr -> do
              let paritionId = C.unPartitionId $ C.crPartition cr
                  currentOffset = C.unOffset $ C.crOffset cr
                  latestOffsetInPartition = HM.lookup paritionId patitionOffsetMap
              if (latestOffsetInPartition <&> fst) > Just currentOffset
                then patitionOffsetMap
                else HM.insert paritionId (currentOffset, cr) patitionOffsetMap
          )
          HM.empty
        . mapMaybe (snd . snd)
        $ M.toList driverLocationUpdatesMap

    calculateAvailableTime' (merchantId, driverId) (timeSeries, _) =
      calculateAvailableTime'' merchantId driverId $ reverse timeSeries

    createUpdatedTimeBuckets oldDriverAvaiabilityBuckets (key@(merchantId, driverId), newAvailabilityBuckets) =
      withLogTag driverId $ traverse createUpdatedTimeBuckets' $ M.toList newAvailabilityBuckets
      where
        createUpdatedTimeBuckets' (innerKey@(bucketStartTime, bucketEndTime), (valToAdd, lastAvailableTime)) = do
          let mbOldBucketAvailableTime = HM.lookup (key, show innerKey) oldDriverAvaiabilityBuckets
          daId <- C.generateGUID
          currTime <- liftIO getCurrentTime
          pure $
            T.DriverAvailability
              { id = daId,
                createdAt = currTime,
                updatedAt = currTime,
                totalAvailableTime = maybe 0 (.totalAvailableTime) mbOldBucketAvailableTime + fromInteger valToAdd,
                ..
              }

calculateAvailableTime'' :: T.MerchantId -> T.DriverId -> [UTCTime] -> Flow T.AvailabilityBucket
calculateAvailableTime'' _ _ [] = pure M.empty
calculateAvailableTime'' merchantId driverId (fstTime : restTimeSeries) = do
  mbLatestAvailabilityRecord <- Q.findLatestByDriverIdAndMerchantId driverId merchantId
  timeBetweenUpdates <- asks (.timeBetweenUpdates)
  granualityPeriodType <- asks (.granualityPeriodType)
  let lastAvailableTime = fromMaybe fstTime $ mbLatestAvailabilityRecord <&> (.lastAvailableTime)
      activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastAvailableTime granualityPeriodType $ filter (> lastAvailableTime) (fstTime : restTimeSeries)
      availabilityInWindow =
        foldl
          ( \acc timePair -> do
              let bucketPair = getBucketPair granualityPeriodType timePair
              flip (M.insert bucketPair) acc $
                case M.lookup bucketPair acc of
                  Just (oldAvailabileTime, _) -> (oldAvailabileTime + getTimeDiffInteger timePair, snd timePair)
                  Nothing -> (getTimeDiffInteger timePair, snd timePair)
          )
          M.empty
          activeTimePairs
  logInfo $ "ActiveTime pairs " <> show activeTimePairs
  logInfo $ "availabilityInWindow " <> show availabilityInWindow
  pure availabilityInWindow
  where
    getBucketPair periodType (startTime, _) = do
      let bucketEndTime = SW.incrementPeriod periodType startTime
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
  availabilityTimeWindowOption <- asks (.availabilityTimeWindowOption)
  lastUpdatedAt <- fromMaybe newUpdatedAt <$> Redis.get (mkLastTimeStampKey driverId)
  unless (lastUpdatedAt > newUpdatedAt) $ do
    Redis.setExp (mkLastTimeStampKey driverId) newUpdatedAt 14400 -- 4 hours
    let activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastUpdatedAt availabilityTimeWindowOption.periodType [lastUpdatedAt, newUpdatedAt]
    mapM_
      ( \(startTime, endTime) -> do
          let valueToAdd = getTimeDiffInteger (startTime, endTime)
          SW.incrementByValueInTimeBucket startTime valueToAdd (mkAvailableTimeKey driverId) availabilityTimeWindowOption
      )
      activeTimePairs
  where
    mkAvailableTimeKey :: Text -> Text
    mkAvailableTimeKey = (<> (mId <> "-available-time"))

    mkLastTimeStampKey :: Text -> Text
    mkLastTimeStampKey = (<> (mId <> "-last-location-update-at"))
