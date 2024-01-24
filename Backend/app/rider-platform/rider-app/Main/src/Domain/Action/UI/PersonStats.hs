{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.PersonStats where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Person as SP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.Person.PersonStats as QPS
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SavedReqLocation as QSRL
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Maps
import Tools.Metrics (CoreMetrics)
import qualified Tools.Whatsapp as WhatsApp

data PersonStatsRes = PersonStatsRes
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

getPersonStats :: (EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => (Id DP.Person, Id Merchant.Merchant) -> m PersonStatsRes
getPersonStats (personId, _) = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  personStats_ <- runInReplica $ QPS.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  now <- getCurrentTime
  completedRidesCount <- runInReplica $ QRide.countRidesByRiderId personId
  when (completedRidesCount > 0 && all (== 0) [personStats_.userCancelledRides, personStats_.completedRides, personStats_.weekendRides, personStats_.weekdayRides, personStats_.offPeakRides, personStats_.eveningPeakRides, personStats_.morningPeakRides, personStats_.weekendPeakRides]) $ do
    SP.backfillPersonStats personId person.merchantOperatingCityId

  personStats <- runInReplica $ QPS.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  latestCompletedRide <- runInReplica $ QRide.findLatestCompletedRide personId
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
    PersonStatsRes
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
      | avgRidesPerWeek >= 1 = REGULAR
      | avgRidesPerWeek >= 0.25 = IRREGULAR
      | otherwise = RARE

    getFrequencyCategory completedRidesInLast30DaysCount
      | completedRidesInLast30DaysCount >= 12 = HIGH
      | completedRidesInLast30DaysCount >= 4 = MID
      | completedRidesInLast30DaysCount >= 1 = LOW
      | otherwise = ZERO
