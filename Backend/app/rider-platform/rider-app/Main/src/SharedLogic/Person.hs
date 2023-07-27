{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Person where

import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar.WeekDate
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonStats as DPS
import qualified Domain.Types.Ride as DR
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow, runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.CacheConfig as SCC
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics (CoreMetrics)

backfillPersonStats :: (EsqDBFlow m r, EsqDBReplicaFlow m r, SCC.CacheFlow m r, CoreMetrics m) => Id DP.Person -> Id Merchant.Merchant -> m DPS.PersonStats
backfillPersonStats personId merchantId = do
  cancelledBookingIds <- runInReplica $ QBooking.findAllCancelledBookingIdsByRider personId
  incrementUserCancelledRidesBy <- runInReplica $ QBCR.countCancelledBookingsByRiderId cancelledBookingIds DBCR.ByUser
  let incrementDriverCancelledRidesBy = length cancelledBookingIds - incrementUserCancelledRidesBy
  completedRides <- runInReplica QRide.findAllCompletedRides
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  let minuteDiffFromUTC = (merchant.timeDiffFromUtc.getSeconds) `div` 60
  now <- getCurrentTime
  let completedRidesCnt = length completedRides
  let (incrementWeekendRidesBy, incrementWeekdayRidesBy) = countWeekdaysAndWeekendsRide completedRides minuteDiffFromUTC
      incrementEveningPeakRides = getWeekdayEveningPeakRides completedRides minuteDiffFromUTC
      incrementMorningPeakRides = getWeekdayMorningPeakRides completedRides minuteDiffFromUTC
      incrementOffPeekRidesBy = completedRidesCnt - (incrementMorningPeakRides + incrementEveningPeakRides)
      personStatsValues =
        DPS.PersonStats
          { personId = personId,
            userCancelledRides = incrementUserCancelledRidesBy,
            driverCancelledRides = incrementDriverCancelledRidesBy,
            completedRides = completedRidesCnt,
            weekendRides = incrementWeekendRidesBy,
            weekdayRides = incrementWeekdayRidesBy,
            offPeakRides = incrementOffPeekRidesBy,
            eveningPeakRides = incrementEveningPeakRides,
            morningPeakRides = incrementMorningPeakRides,
            weekendPeakRides = getWeekendPeakRides completedRides minuteDiffFromUTC,
            updatedAt = now
          }
  pure personStatsValues

countWeekdaysAndWeekendsRide :: [DR.Ride] -> Int -> (Int, Int)
countWeekdaysAndWeekendsRide rides minuteDiffFromUTC =
  let countWeekdaysAndWeekends' =
        foldl'
          ( \(weekendCount, weekdayCount) ride ->
              if isWeekend ride.createdAt minuteDiffFromUTC
                then (weekendCount + 1, weekdayCount)
                else (weekendCount, weekdayCount + 1)
          )
          (0, 0)
   in countWeekdaysAndWeekends' rides

getWeekdayMorningPeakRides :: [DR.Ride] -> Int -> Int
getWeekdayMorningPeakRides rides minuteDiffFromUTC =
  let countRides = foldl' (\cnt ride -> if checkMorningPeakInWeekday ride.createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides rides

checkMorningPeakInWeekday :: UTCTime -> Int -> Bool
checkMorningPeakInWeekday utcTime minuteDiffFromUTC = not (isWeekend utcTime minuteDiffFromUTC) && within (convertTimeZone utcTime minuteDiffFromUTC) (TimeOfDay 8 0 0) (TimeOfDay 11 0 0)

getWeekdayEveningPeakRides :: [DR.Ride] -> Int -> Int
getWeekdayEveningPeakRides rides minuteDiffFromUTC =
  let countRides = foldl' (\cnt ride -> if checkEveningPeakInWeekday ride.createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides rides

checkEveningPeakInWeekday :: UTCTime -> Int -> Bool
checkEveningPeakInWeekday utcTime minuteDiffFromUTC = not (isWeekend utcTime minuteDiffFromUTC) && within (convertTimeZone utcTime minuteDiffFromUTC) (TimeOfDay 16 0 0) (TimeOfDay 19 0 0)

getWeekendPeakRides :: [DR.Ride] -> Int -> Int
getWeekendPeakRides rides minuteDiffFromUTC =
  let countRides = foldl' (\cnt ride -> if checkWeekendPeak ride.createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides rides

checkWeekendPeak :: UTCTime -> Int -> Bool
checkWeekendPeak utcTime minuteDiffFromUTC = isWeekend utcTime minuteDiffFromUTC && within (convertTimeZone utcTime minuteDiffFromUTC) (TimeOfDay 12 30 0) (TimeOfDay 19 30 0)

within :: LocalTime -> TimeOfDay -> TimeOfDay -> Bool
within localDateTime startTime endTime =
  let time = localTimeOfDay localDateTime
   in time >= startTime && time <= endTime

isWeekend :: UTCTime -> Int -> Bool
isWeekend utcT minuteDiffFromUTC =
  let locTime = convertTimeZone utcT minuteDiffFromUTC
      (_, _, weekDay) = toWeekDate $ localDay locTime
   in weekDay > 5

convertTimeZone :: UTCTime -> Int -> LocalTime
convertTimeZone timeInUTC minuteDiffFromUTC = utcToLocalTime (minutesToTimeZone minuteDiffFromUTC) timeInUTC
