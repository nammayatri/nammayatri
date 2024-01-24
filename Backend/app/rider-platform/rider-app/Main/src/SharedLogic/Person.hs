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
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonStats as DPS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Person.PersonStats as QP
import Tools.Error
import Tools.Metrics (CoreMetrics)

backfillPersonStats :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, CoreMetrics m) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m ()
backfillPersonStats personId merchantOpCityid = do
  cancelledBookingIds <- runInReplica $ QBooking.findAllCancelledBookingIdsByRider personId
  userCancelledRides <- runInReplica $ QBCR.countCancelledBookingsByBookingIds cancelledBookingIds DBCR.ByUser
  driverCancelledRides <- runInReplica $ QBCR.countCancelledBookingsByBookingIds cancelledBookingIds DBCR.ByDriver
  completedBookings <- runInReplica $ QBooking.findByRiderIdAndStatus personId [DB.COMPLETED]
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityid >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityid.getId)
  let minuteDiffFromUTC = (riderConfig.timeDiffFromUtc.getSeconds) `div` 60
  now <- getCurrentTime
  let completedRidesCnt = length completedBookings
  let (weekendRides, weekdayRides) = countWeekdaysAndWeekendsRide completedBookings minuteDiffFromUTC
      eveningPeakRides = getWeekdayEveningPeakRides completedBookings minuteDiffFromUTC
      morningPeakRides = getWeekdayMorningPeakRides completedBookings minuteDiffFromUTC
      weekendPeakRides = getWeekendPeakRides completedBookings minuteDiffFromUTC
      offPeakRides = completedRidesCnt - (morningPeakRides + eveningPeakRides + weekendPeakRides)
      personStatsValues =
        DPS.PersonStats
          { completedRides = completedRidesCnt,
            updatedAt = now,
            ..
          }
  QP.incrementOrSetPersonStats personStatsValues

countWeekdaysAndWeekendsRide :: [DB.Booking] -> Int -> (Int, Int)
countWeekdaysAndWeekendsRide bookings minuteDiffFromUTC =
  let countWeekdaysAndWeekends' =
        foldl'
          ( \(weekendCount, weekdayCount) booking ->
              if isWeekend booking.createdAt minuteDiffFromUTC
                then (weekendCount + 1, weekdayCount)
                else (weekendCount, weekdayCount + 1)
          )
          (0, 0)
   in countWeekdaysAndWeekends' bookings

getWeekdayMorningPeakRides :: [DB.Booking] -> Int -> Int
getWeekdayMorningPeakRides bookings minuteDiffFromUTC =
  let countRides = foldl' (\cnt booking -> if checkMorningPeakInWeekday booking.createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides bookings

checkMorningPeakInWeekday :: UTCTime -> Int -> Bool
checkMorningPeakInWeekday utcTime minuteDiffFromUTC = not (isWeekend utcTime minuteDiffFromUTC) && within (convertTimeZone utcTime minuteDiffFromUTC) (TimeOfDay 8 0 0) (TimeOfDay 11 0 0)

getWeekdayEveningPeakRides :: [DB.Booking] -> Int -> Int
getWeekdayEveningPeakRides bookings minuteDiffFromUTC =
  let countRides = foldl' (\cnt booking -> if checkEveningPeakInWeekday booking.createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides bookings

checkEveningPeakInWeekday :: UTCTime -> Int -> Bool
checkEveningPeakInWeekday utcTime minuteDiffFromUTC = not (isWeekend utcTime minuteDiffFromUTC) && within (convertTimeZone utcTime minuteDiffFromUTC) (TimeOfDay 16 0 0) (TimeOfDay 19 0 0)

getWeekendPeakRides :: [DB.Booking] -> Int -> Int
getWeekendPeakRides bookings minuteDiffFromUTC =
  let countRides = foldl' (\cnt booking -> if checkWeekendPeak booking.createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides bookings

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

getName :: DP.Person -> Text
getName person = (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName)
