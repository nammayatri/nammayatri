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
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Environment
import EulerHS.Prelude
import qualified Kafka.Consumer as C
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Common as C hiding (Offset)
import qualified Kernel.Types.SlidingWindowCounters as SWT
import Kernel.Utils.Logging (logInfo)
import qualified Kernel.Utils.SlidingWindowCounters as SW

getTimeDiffInteger :: SWT.TimePair -> Integer
getTimeDiffInteger (startTime, endTime) = floor $ diffUTCTime endTime startTime

createOrUpdateDriverAvailability :: T.MerchantId -> T.DriverId -> SWT.TimePair -> (Integer, T.LastAvailableTime) -> Flow ()
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

calculateAvailableTime :: T.MerchantId -> T.DriverId -> C.KafkaConsumer -> ([UTCTime], Maybe ConsumerRecordD) -> Flow ()
calculateAvailableTime _ _ _ ([], Nothing) = pure ()
calculateAvailableTime _ _ _ ([], Just lastCR) = logInfo $ "Should never reach here, no locationupdates but kafka consumer records , last consumer record: " <> show lastCR
calculateAvailableTime _ _ _ (locationUpdatesTimeSeries, Nothing) = logInfo $ "Should never reach here, locationupdates but no kafka consumer record, time series: " <> show locationUpdatesTimeSeries
calculateAvailableTime merchantId driverId kc (fstTime : restTimeSeries, Just lastCR) = do
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
  void $ M.traverseWithKey (createOrUpdateDriverAvailability merchantId driverId) availabilityInWindow
  void $ C.commitOffsetMessage C.OffsetCommit kc lastCR
  where
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
    Redis.setExp (mkLastTimeStampKey driverId) newUpdatedAt 14400 -- 4 hours
    let activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastUpdatedAt availabilityTimeWindowOption.periodType [lastUpdatedAt, newUpdatedAt]
    mapM_
      ( \(startTime, endTime) -> do
          let valueToAdd = getTimeDiffInteger (startTime, endTime)
          SW.incrementByValueInTimeBucket startTime valueToAdd (mkAvailableTimeKey driverId) availabilityTimeWindowOption
      )
      activeTimePairs
  where
    mkAvailableTimeKey :: T.DriverId -> Text
    mkAvailableTimeKey = (<> (mId <> "-available-time"))

    mkLastTimeStampKey :: T.DriverId -> Text
    mkLastTimeStampKey = (<> (mId <> "-last-location-update-at"))
