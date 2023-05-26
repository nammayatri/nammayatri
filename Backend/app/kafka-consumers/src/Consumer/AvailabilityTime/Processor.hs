{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.AvailabilityTime.Processor
  ( processData,
    calculateAvailableTime,
  )
where

import qualified Consumer.AvailabilityTime.Storage.Queries as Q
import qualified Consumer.AvailabilityTime.Types as T
import qualified Data.HashMap as HM
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Environment
import EulerHS.Prelude
import qualified Kafka.Consumer as C
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Common as C hiding (Offset)
import qualified Kernel.Types.SlidingWindowCounters as SWT
import Kernel.Utils.Common (withLogTag)
import Kernel.Utils.Logging (logInfo, logTagDebug)
import qualified Kernel.Utils.SlidingWindowCounters as SW
import qualified SharedLogic.DriverPool as DP

getTimeDiffInteger :: SWT.TimePair -> Integer
getTimeDiffInteger (startTime, endTime) = floor $ diffUTCTime endTime startTime

createOrUpdateDriverAvailability :: T.MerchantId -> T.DriverId -> Text -> SWT.TimePair -> (Integer, T.LastAvailableTime) -> Flow ()
createOrUpdateDriverAvailability merchantId driverId lastAvailableTimeCacheKey (bucketStartTime, bucketEndTime) (valToAdd, lastAvailableTime) = do
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
  Redis.setExp lastAvailableTimeCacheKey (lastAvailableTime, False) 28800 -- 8 hours
  DB.runNoTransaction $ Q.createOrUpdateDriverAvailability newAvailabilityEntry
  Redis.setExp lastAvailableTimeCacheKey (lastAvailableTime, True) 28800 -- 8 hours

calculateAvailableTime :: C.KafkaConsumer -> Map (T.MerchantId, T.DriverId) ([UTCTime], Maybe ConsumerRecordD) -> Flow ()
calculateAvailableTime kafkaConsumer driverLocationUpdatesMap = do
  void $ M.traverseWithKey calAvalTime driverLocationUpdatesMap
  mapM_ (C.commitOffsetMessage C.OffsetCommit kafkaConsumer) getLatestCRInEachPartition
  where
    calAvalTime (driverId, merchantId) (timeSeries, _) =
      calculateAvailableTime' merchantId driverId $ reverse timeSeries

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

calculateAvailableTime' :: T.MerchantId -> T.DriverId -> [UTCTime] -> Flow ()
calculateAvailableTime' _ _ [] = pure ()
calculateAvailableTime' merchantId driverId (fstTime : restTimeSeries) = withLogTag driverId $
  Redis.whenWithLockRedis (mkLastTimeStampKey <> ":lock") 10 $ do
    cachedLastAvailableTime <- Redis.get mkLastTimeStampKey
    lastAvailableTime <-
      case cachedLastAvailableTime of
        Just (unsureLastCommitedTime, True) -> pure unsureLastCommitedTime
        _ -> do
          whenJust cachedLastAvailableTime $ \_ -> logTagDebug "DRIVER_AVAILABILITY:LAT" "not commited to Redis in previous iteration"
          lstAvalTime <- maybe fstTime (.lastAvailableTime) <$> DB.runInReplica (Q.findLatestByDriverIdAndMerchantId driverId merchantId)
          Redis.setExp mkLastTimeStampKey (lstAvalTime, True) 28800 -- 8 hours
          pure lstAvalTime
    timeBetweenUpdates <- asks (.timeBetweenUpdates)
    granualityPeriodType <- asks (.granualityPeriodType)
    let activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastAvailableTime granualityPeriodType $ filter (> lastAvailableTime) (fstTime : restTimeSeries)
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
    void $ M.traverseWithKey (createOrUpdateDriverAvailability merchantId driverId mkLastTimeStampKey) availabilityInWindow
  where
    mkLastTimeStampKey = "DA:LAT:" <> driverId
    getBucketPair periodType (startTime, _) = do
      let bucketEndTime = SW.incrementPeriod periodType startTime
      let bucketStartTime = flip addUTCTime bucketEndTime . fromInteger $ -1 * SW.convertPeriodTypeToSeconds periodType
      (bucketStartTime, bucketEndTime)

mkPairsWithLessThenThreshold :: Integer -> T.LastAvailableTime -> SWT.PeriodType -> [UTCTime] -> [SWT.TimePair]
mkPairsWithLessThenThreshold timeBetweenUpdates lastAvailableTime granualityPeriodType =
  snd
    . foldl
      ( \(oldTime, newList) newTime ->
          if (floor (diffUTCTime newTime oldTime) :: Integer) <= timeBetweenUpdates
            then (newTime, newList <> SW.splitOnPeriodGranuality granualityPeriodType (oldTime, newTime))
            else (newTime, newList)
      )
      (lastAvailableTime, [])

processData :: T.LocationUpdates -> T.DriverId -> Flow ()
processData T.LocationUpdates {..} driverId = do
  let newUpdatedAt = ts
  timeBetweenUpdates <- asks (.timeBetweenUpdates)
  availabilityTimeWindowOption <- asks (.availabilityTimeWindowOption)
  lastUpdatedAt <- fromMaybe newUpdatedAt <$> Redis.get (mkLastTimeStampKey driverId)
  unless (lastUpdatedAt > newUpdatedAt) $ do
    Redis.setExp (mkLastTimeStampKey driverId) newUpdatedAt 28800 -- 8 hours
    let activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastUpdatedAt availabilityTimeWindowOption.periodType [lastUpdatedAt, newUpdatedAt]
    mapM_
      ( \(startTime, endTime) -> do
          let valueToAdd = getTimeDiffInteger (startTime, endTime)
          SW.incrementByValueInTimeBucket startTime valueToAdd (DP.mkAvailableTimeKey driverId) availabilityTimeWindowOption
      )
      activeTimePairs
  where
    mkLastTimeStampKey :: T.DriverId -> Text
    mkLastTimeStampKey = (<> (mId <> "-last-location-update-at"))
