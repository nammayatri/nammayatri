{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CleverTap where

import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar.WeekDate
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonStats as DPS
import qualified Domain.Types.Ride as DR
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.Person.PersonStats as QPS
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SavedReqLocation as QSRL
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Maps
import qualified Tools.Whatsapp as WhatsApp

data GetCleverTapDataRes = GetCleverTapDataRes
  { riderId :: Id DP.Person,
    signupDate :: UTCTime,
    email :: Maybe Text,
    isWhatsAppOptInStatus :: Bool,
    lifetimeRides :: Int,
    lastRideTaken :: Maybe UTCTime,
    userCancellationRate :: Float,
    frequencyCategory :: FrequencyCategory,
    isChurnedUser :: Bool,
    userCategory :: UserCategory,
    isBlocked :: Bool,
    latestSearch :: Maybe UTCTime,
    latestSearchFrom :: Maybe LatLong,
    emergencyContactsNum :: Int,
    overalCancellationRate :: Float,
    favoriteLocationsNum :: Int,
    weekdayMorningPeakRidesRate :: Float,
    weekdayEveningPeakRidesRate :: Float,
    offPeakRidesRate :: Float,
    weekendRidesRate :: Float,
    weekdayRidesRate :: Float,
    weekendPeakRideRate :: Float,
    commonAppUseCase :: AppUseCase
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AppUseCase = WEEKDAY | WEEKEND | OCCASIONAL | MISCELLANEOUS
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FrequencyCategory = HIGH | MID | LOW | ZERO
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UserCategory = POWER | REGULAR | IRREGULAR | RARE
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getCleverTapData :: (EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r) => (Id DP.Person, Id Merchant.Merchant) -> Maybe LocalZone -> m GetCleverTapDataRes
getCleverTapData (personId, _) mbLocalTZone = do
  let localTZone = fromMaybe IST mbLocalTZone
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  personStats_ <- runInReplica $ QPS.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  now <- getCurrentTime
  when (all (== 0) [personStats_.userCancelledRides, personStats_.completedRides, personStats_.weekendRides, personStats_.weekdayRides, personStats_.offPeakRides, personStats_.eveningPeakRides, personStats_.morningPeakRides, personStats_.weekendPeakRides]) $ do
    personStatsValues <- backfillPersonStats personId (Just localTZone)
    Esq.runNoTransaction $ QPS.incrementOrSetPersonStats personStatsValues

  personStats <- runInReplica $ QPS.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  latestCompletedRide <- runInReplica $ QRide.findLatestCompletedRide personId
  completedRidesCount <- runInReplica $ QRide.countRidesByRiderId personId
  favLocNum <- runInReplica $ QSRL.countAllByRiderId personId
  completedRidesInLast30DaysCount <- runInReplica $ QRide.countRidesFromDateToNowByRiderId personId (negate days30 `addUTCTime` now)
  latestSearch <- runInReplica $ QSR.findLatestSearchRequest personId
  decEmail <- decrypt `mapM` person.email
  emergencyContactsNum <- length <$> runInReplica (QPDEN.findAllByPersonId personId)
  let userCancellationRate =
        fromIntegral personStats.userCancelledRides
          / fromIntegral (personStats.userCancelledRides + personStats.completedRides)
      overalCancellationRate =
        fromIntegral (personStats.userCancelledRides + personStats.driverCancelledRides)
          / fromIntegral (personStats.userCancelledRides + personStats.driverCancelledRides + personStats.completedRides)
      weekdayMorningPeakRidesRate =
        fromIntegral personStats.morningPeakRides
          / fromIntegral personStats.completedRides
      weekdayEveningPeakRidesRate =
        fromIntegral personStats.eveningPeakRides
          / fromIntegral personStats.completedRides
      offPeakRidesRate =
        fromIntegral personStats.offPeakRides
          / fromIntegral personStats.completedRides
      weekendRidesRate =
        fromIntegral personStats.weekendRides
          / fromIntegral personStats.completedRides
      weekdayRidesRate =
        fromIntegral personStats.weekdayRides
          / fromIntegral personStats.completedRides
      weekendPeakRideRate =
        fromIntegral personStats.weekendPeakRides
          / fromIntegral personStats.completedRides
      daysFromRegDate :: Int = roundToIntegral $ diffUTCTime now person.createdAt / 60 / 60 / 24
      avgRidesPerWeek :: Float = fromIntegral completedRidesCount / (fromIntegral daysFromRegDate / 7)

  return $
    GetCleverTapDataRes
      { riderId = person.id,
        signupDate = person.createdAt,
        email = decEmail,
        isWhatsAppOptInStatus = Just WhatsApp.OPT_IN == person.whatsappNotificationEnrollStatus,
        lifetimeRides = completedRidesCount,
        lastRideTaken = latestCompletedRide <&> (.createdAt),
        frequencyCategory = getFrequencyCategory completedRidesInLast30DaysCount,
        isChurnedUser = maybe False (isChurned now) latestCompletedRide,
        userCategory = getUserCategory avgRidesPerWeek,
        isBlocked = person.blocked,
        latestSearch = latestSearch <&> (.createdAt),
        latestSearchFrom = latestSearch <&> getCoordinates . (.fromLocation),
        favoriteLocationsNum = favLocNum,
        commonAppUseCase = getCommonAppUseCase weekdayRidesRate weekendRidesRate completedRidesCount avgRidesPerWeek,
        ..
      }
  where
    days30 :: NominalDiffTime = 30 * 24 * 60 * 60
    isChurned now latestCompletedRide = do
      diffUTCTime now latestCompletedRide.createdAt > days30
    getCommonAppUseCase weekdayRidesRate weekendRidesRate completedRidesCount avgRidesPerWeek
      | weekdayRidesRate > 0.6 && completedRidesCount > 4 = WEEKDAY
      | weekendRidesRate > 0.6 && completedRidesCount > 4 = WEEKEND
      | avgRidesPerWeek >= 0.1 && avgRidesPerWeek <= 0.25 = OCCASIONAL
      | otherwise = MISCELLANEOUS

    getUserCategory avgRidesPerWeek
      | avgRidesPerWeek >= 3 = POWER
      | avgRidesPerWeek >= 1 && avgRidesPerWeek < 3 = REGULAR
      | avgRidesPerWeek >= 0.25 && avgRidesPerWeek < 1 = IRREGULAR
      | otherwise = RARE

    getFrequencyCategory completedRidesInLast30DaysCount
      | completedRidesInLast30DaysCount >= 12 = HIGH
      | completedRidesInLast30DaysCount >= 4 && completedRidesInLast30DaysCount < 12 = MID
      | completedRidesInLast30DaysCount >= 1 && completedRidesInLast30DaysCount < 4 = LOW
      | otherwise = ZERO

backfillPersonStats :: EsqDBReplicaFlow m r => Id DP.Person -> Maybe LocalZone -> m DPS.PersonStats
backfillPersonStats personId mbLocalTZone = do
  let localTZone = fromMaybe IST mbLocalTZone
  cancelledBookingIds <- runInReplica $ QBooking.findAllCancelledBookingIdsByRider personId
  incrementUserCancelledRidesBy <- runInReplica $ QBCR.countCancelledBookingsByRiderId cancelledBookingIds DBCR.ByUser
  incrementDriverCancelledRidesBy <- runInReplica $ QBCR.countCancelledBookingsByRiderId cancelledBookingIds DBCR.ByDriver
  completedRides <- runInReplica QRide.findAllCompletedRides
  now <- getCurrentTime
  let completedRidesCnt = length completedRides
  let (incrementWeekendRidesBy, incrementWeekdayRidesBy) = countWeekdaysAndWeekendsRide completedRides localTZone
      incrementEveningPeakRides = getWeekdayEveningPeakRides completedRides localTZone
      incrementMorningPeakRides = getWeekdayMorningPeakRides completedRides localTZone
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
            weekendPeakRides = getWeekendPeakRides completedRides localTZone,
            updatedAt = now
          }
  pure personStatsValues

countWeekdaysAndWeekendsRide :: [DR.Ride] -> LocalZone -> (Int, Int)
countWeekdaysAndWeekendsRide rides localTZone =
  let countWeekdaysAndWeekends' =
        foldl'
          ( \(weekendCount, weekdayCount) ride ->
              if isWeekend ride.createdAt localTZone
                then (weekendCount + 1, weekdayCount)
                else (weekendCount, weekdayCount + 1)
          )
          (0, 0)
   in countWeekdaysAndWeekends' rides

getWeekdayMorningPeakRides :: [DR.Ride] -> LocalZone -> Int
getWeekdayMorningPeakRides rides localTZone =
  let countRides = foldl' (\cnt ride -> if checkMorningPeakInWeekday ride.createdAt localTZone then cnt + 1 else cnt) 0 in countRides rides

checkMorningPeakInWeekday :: UTCTime -> LocalZone -> Bool
checkMorningPeakInWeekday utcTime locZone = not (isWeekend utcTime locZone) && within (convertTimeZone utcTime locZone) (TimeOfDay 8 0 0) (TimeOfDay 11 0 0)

getWeekdayEveningPeakRides :: [DR.Ride] -> LocalZone -> Int
getWeekdayEveningPeakRides rides localTZone =
  let countRides = foldl' (\cnt ride -> if checkEveningPeakInWeekday ride.createdAt localTZone then cnt + 1 else cnt) 0 in countRides rides

checkEveningPeakInWeekday :: UTCTime -> LocalZone -> Bool
checkEveningPeakInWeekday utcTime locZone = not (isWeekend utcTime locZone) && within (convertTimeZone utcTime locZone) (TimeOfDay 16 0 0) (TimeOfDay 19 0 0)

getWeekendPeakRides :: [DR.Ride] -> LocalZone -> Int
getWeekendPeakRides rides localTZone =
  let countRides = foldl' (\cnt ride -> if checkWeekendPeak ride.createdAt localTZone then cnt + 1 else cnt) 0 in countRides rides

checkWeekendPeak :: UTCTime -> LocalZone -> Bool
checkWeekendPeak utcTime locZone = isWeekend utcTime locZone && within (convertTimeZone utcTime locZone) (TimeOfDay 12 30 0) (TimeOfDay 19 30 0)

within :: LocalTime -> TimeOfDay -> TimeOfDay -> Bool
within localDateTime startTime endTime =
  let time = localTimeOfDay localDateTime
   in time >= startTime && time <= endTime

isWeekend :: UTCTime -> LocalZone -> Bool
isWeekend utcTime localZone =
  let locTime = convertTimeZone utcTime localZone
      (_, _, weekDay) = toWeekDate $ localDay locTime
   in weekDay > 5
