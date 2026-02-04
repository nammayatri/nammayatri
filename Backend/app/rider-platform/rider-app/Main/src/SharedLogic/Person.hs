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
import qualified Domain.Types.BookingStatus as DB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.SafetySettings as DSafety
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Storage.Esqueleto (EsqDBFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fork, fromMaybeM, getCurrentTime)
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.BookingCancellationReason as CHBCR
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QP
import qualified Storage.Queries.SafetySettings as QSafety
import Tools.Error
import Tools.Metrics (CoreMetrics)

personRedisKey :: Id DP.Person -> Text
personRedisKey pId = "person_stats:" <> pId.getId <> ":"

backfillPersonStats :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, CoreMetrics m, ClickhouseFlow m r) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m ()
backfillPersonStats personId merchantOpCityid = do
  personStatsData <- getBackfillPersonStatsData personId merchantOpCityid
  QP.incrementOrSetPersonStats personStatsData

getBackfillPersonStatsData :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, CoreMetrics m, ClickhouseFlow m r) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m DPS.PersonStats
getBackfillPersonStatsData personId merchantOpCityid = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  maxBookingTimeCancelled <- CHB.findMaxTimeForCancelledBookingByRiderId personId person.createdAt
  (userCancelledRides, driverCancelledRides) <- CHBCR.countCancelledBookingsByRiderIdGroupByByUserAndDriver personId person.createdAt
  completedBookingsCreatedAt <- CHB.findByRiderIdAndStatus personId DB.COMPLETED person.createdAt
  let maxBookingTimeCompleted = foldl' max person.createdAt completedBookingsCreatedAt
  let maxBookingTime = max maxBookingTimeCancelled maxBookingTimeCompleted
  Hedis.setExp (personRedisKey personId) maxBookingTime 43200
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityid.getId, txnId = Nothing}) >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityid.getId)
  let minuteDiffFromUTC = (riderConfig.timeDiffFromUtc.getSeconds) `div` 60
  now <- getCurrentTime
  let completedRidesCnt = length completedBookingsCreatedAt
  let (weekendRides, weekdayRides) = countWeekdaysAndWeekendsRide completedBookingsCreatedAt minuteDiffFromUTC
      eveningPeakRides = getWeekdayEveningPeakRides completedBookingsCreatedAt minuteDiffFromUTC
      morningPeakRides = getWeekdayMorningPeakRides completedBookingsCreatedAt minuteDiffFromUTC
      weekendPeakRides = getWeekendPeakRides completedBookingsCreatedAt minuteDiffFromUTC
      offPeakRides = completedRidesCnt - (morningPeakRides + eveningPeakRides + weekendPeakRides)
      personStatsValues =
        DPS.PersonStats
          { completedRides = completedRidesCnt,
            createdAt = now,
            updatedAt = now,
            referralCount = 0,
            ticketsBookedInEvent = Just 0,
            referralAmountPaid = 0,
            referralEarnings = 0,
            referredByEarnings = 0,
            validActivations = 0,
            referredByEarningsPayoutStatus = Nothing,
            backlogPayoutStatus = Nothing,
            backlogPayoutAmount = 0,
            isBackfilled = Just True,
            ..
          }
  return personStatsValues

countWeekdaysAndWeekendsRide :: [UTCTime] -> Int -> (Int, Int)
countWeekdaysAndWeekendsRide bookingsCreatedAt minuteDiffFromUTC =
  let countWeekdaysAndWeekends' =
        foldl'
          ( \(weekendCount, weekdayCount) createdAt ->
              if isWeekend createdAt minuteDiffFromUTC
                then (weekendCount + 1, weekdayCount)
                else (weekendCount, weekdayCount + 1)
          )
          (0, 0)
   in countWeekdaysAndWeekends' bookingsCreatedAt

getWeekdayMorningPeakRides :: [UTCTime] -> Int -> Int
getWeekdayMorningPeakRides bookingsCreatedAt minuteDiffFromUTC =
  let countRides = foldl' (\cnt createdAt -> if checkMorningPeakInWeekday createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides bookingsCreatedAt

checkMorningPeakInWeekday :: UTCTime -> Int -> Bool
checkMorningPeakInWeekday utcTime minuteDiffFromUTC = not (isWeekend utcTime minuteDiffFromUTC) && within (convertTimeZone utcTime minuteDiffFromUTC) (TimeOfDay 8 0 0) (TimeOfDay 11 0 0)

getWeekdayEveningPeakRides :: [UTCTime] -> Int -> Int
getWeekdayEveningPeakRides bookingsCreatedAt minuteDiffFromUTC =
  let countRides = foldl' (\cnt createdAt -> if checkEveningPeakInWeekday createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides bookingsCreatedAt

checkEveningPeakInWeekday :: UTCTime -> Int -> Bool
checkEveningPeakInWeekday utcTime minuteDiffFromUTC = not (isWeekend utcTime minuteDiffFromUTC) && within (convertTimeZone utcTime minuteDiffFromUTC) (TimeOfDay 16 0 0) (TimeOfDay 19 0 0)

getWeekendPeakRides :: [UTCTime] -> Int -> Int
getWeekendPeakRides bookingsCreatedAt minuteDiffFromUTC =
  let countRides = foldl' (\cnt createdAt -> if checkWeekendPeak createdAt minuteDiffFromUTC then cnt + 1 else cnt) 0 in countRides bookingsCreatedAt

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

checkSafetyCenterDisabled :: (EsqDBFlow m r, CacheFlow m r) => DP.Person -> DSafety.SafetySettings -> m Bool
checkSafetyCenterDisabled person safetySettings = do
  let isPermanentBlock = safetySettings.falseSafetyAlarmCount >= 6
  case safetySettings.safetyCenterDisabledOnDate of
    Nothing -> return False
    Just safetyCenterDisabledOnDate -> do
      if isPermanentBlock
        then return True
        else do
          now <- getCurrentTime
          riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, txnId = Nothing}) >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
          let unblockAfterDays = (intToNominalDiffTime riderConfig.autoUnblockSafetyCenterAfterDays) * 24 * 60 * 60
          if diffUTCTime now safetyCenterDisabledOnDate > unblockAfterDays
            then do
              fork "" $ QSafety.updateSafetyCenterBlockingCounter person.id Nothing Nothing
              return False
            else return True
