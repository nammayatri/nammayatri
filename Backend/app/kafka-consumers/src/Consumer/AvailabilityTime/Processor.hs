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
import Data.List (nub)
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Environment
import EulerHS.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Common as C hiding (Offset)
import qualified Kernel.Types.SlidingWindowCounters as SWT
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
  Q.createOrUpdateDriverAvailability newAvailabilityEntry
  Redis.setExp lastAvailableTimeCacheKey (lastAvailableTime, True) 28800 -- 8 hours

calculateAvailableTime :: T.MerchantId -> T.DriverId -> [UTCTime] -> Flow ()
calculateAvailableTime _ _ [] = pure ()
calculateAvailableTime merchantId driverId (fstTime : restTimeSeries) = do
  cachedLastAvailableTime <- Redis.get mkLastTimeStampKey
  lastAvailableTime <-
    case cachedLastAvailableTime of
      Just (unsureLastCommitedTime, True) -> pure unsureLastCommitedTime
      _ -> do
        whenJust cachedLastAvailableTime $ \_ -> logTagDebug "DRIVER_AVAILABILITY:LAT" "not commited to Redis in previous iteration"
        -- lstAvalTime <- maybe fstTime (.lastAvailableTime) <$> DB.runInReplica (Q.findLatestByDriverIdAndMerchantId driverId merchantId)
        lstAvalTime <- maybe fstTime (.lastAvailableTime) <$> (Q.findLatestByDriverIdAndMerchantId driverId merchantId)
        Redis.setExp mkLastTimeStampKey (lstAvalTime, True) 28800 -- 8 hours
        pure lstAvalTime
  timeBetweenUpdates <- asks (.timeBetweenUpdates)
  granualityPeriodType <- asks (.granualityPeriodType)
  let activeTimePairs = mkPairsWithLessThenThreshold timeBetweenUpdates lastAvailableTime granualityPeriodType $ sort $ nub $ filter (> lastAvailableTime) (fstTime : restTimeSeries)
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
  logInfo $ "Location Updates Timeseries: " <> show (fstTime : restTimeSeries)
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
