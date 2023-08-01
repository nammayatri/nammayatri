{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.UI.DriverProfileSummary where

import qualified Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Feedback.Feedback (FeedbackBadge)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DR
import Domain.Types.Vehicle (VehicleAPIEntity)
import qualified Domain.Types.Vehicle as SV
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as BQ
import Storage.Queries.DriverStats
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as FPQ
import qualified Storage.Queries.Feedback.FeedbackBadge as QFB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Rating as QRating
import qualified Storage.Queries.Ride as RQ
import qualified Storage.Queries.Vehicle as QVehicle

data DriverProfleSummaryRes = DriverProfleSummaryRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    totalRidesAssigned :: Int,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    totalDistanceTravelled :: Meters,
    rating :: Maybe Centesimal,
    totalUsersRated :: Int,
    language :: Maybe Maps.Language,
    alternateNumber :: Maybe Text,
    gender :: Maybe SP.Gender,
    driverSummary :: DriverInfo.DriverSummary,
    missedOpp :: DriverInfo.DriverMissedOpp,
    feedbackBadges :: DriverInfo.DriverBadges,
    languagesSpoken :: Maybe [Text],
    hometown :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

getDriverProfileSummary :: (Esq.EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant) -> m DriverProfleSummaryRes
getDriverProfileSummary (driverId, _) = do
  person <- Esq.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  vehicleMB <- Esq.runInReplica $ QVehicle.findById person.id
  decMobNum <- mapM decrypt person.mobileNumber
  decaltMobNum <- mapM decrypt person.alternateMobileNumber
  driverStats_ <- Esq.runInReplica $ QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  when (driverStats_.totalEarnings == 0 && driverStats_.bonusEarned == 0 && driverStats_.lateNightTrips == 0 && driverStats_.earningsMissed == 0) do
    allRides <- Esq.runInReplica $ RQ.findAllRidesByDriverId driverId
    let completedRides = filter ((== DR.COMPLETED) . (.status)) allRides
        farePramIds = mapMaybe (.fareParametersId) completedRides
    lateNightTripsCount <- Esq.runInReplica $ FPQ.findAllLateNightRides farePramIds
    cancelledBookingIds <- Esq.runInReplica $ RQ.findCancelledBookingId (cast person.id)
    missedEarnings <- Esq.runInReplica $ BQ.findFareForCancelledBookings cancelledBookingIds
    driverSelectedFare <- Esq.runInReplica $ FPQ.findDriverSelectedFareEarnings farePramIds
    customerExtraFee <- Esq.runInReplica $ FPQ.findCustomerExtraFees farePramIds
    let bonusEarnings = driverSelectedFare + customerExtraFee + Money (length farePramIds * 10)
        totalEarnings = sum $ map (fromMaybe 0 . (.fare)) completedRides
    Esq.runNoTransaction $ do
      incrementTotalEarningsAndBonusEarnedAndLateNightTrip (cast person.id) totalEarnings bonusEarnings lateNightTripsCount
      setMissedEarnings (cast person.id) missedEarnings
  driverStats <- Esq.runInReplica $ QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  feedbackBadgeList <- Esq.runInReplica $ QFB.findAllFeedbackBadgeForDriver person.id
  totalUsersRated <- Esq.runInReplica $ QRating.findAllRatingUsersCountByPerson driverId

  let driverSummary =
        DriverInfo.DriverSummary
          { totalEarnings = driverStats.totalEarnings,
            bonusEarned = driverStats.bonusEarned,
            totalCompletedTrips = driverStats.totalRides,
            lateNightTrips = driverStats.lateNightTrips,
            lastRegistered = person.createdAt
          }
  let missedOpp =
        DriverInfo.DriverMissedOpp
          { cancellationRate = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int),
            ridesCancelled = fromMaybe 0 driverStats.ridesCancelled,
            totalRides = driverStats.totalRides,
            missedEarnings = driverStats.earningsMissed
          }
  return $
    DriverProfleSummaryRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        totalRidesAssigned = fromMaybe 0 driverStats.totalRidesAssigned,
        mobileNumber = decMobNum,
        rating = person.rating,
        linkedVehicle = SV.makeVehicleAPIEntity <$> vehicleMB,
        totalDistanceTravelled = driverStats.totalDistance,
        language = person.language,
        alternateNumber = decaltMobNum,
        gender = Just person.gender,
        feedbackBadges = generateFeedbackBadge feedbackBadgeList,
        languagesSpoken = person.languagesSpoken,
        hometown = person.hometown,
        ..
      }
  where
    nonZero Nothing = 1
    nonZero (Just a)
      | a <= 0 = 1
      | otherwise = a

    generateFeedbackBadge :: [FeedbackBadge] -> DriverInfo.DriverBadges
    generateFeedbackBadge list =
      DriverInfo.DriverBadges
        { driverBadges = makeBadgeList <$> list
        }

    makeBadgeList :: FeedbackBadge -> DriverInfo.Badges
    makeBadgeList fBadge = DriverInfo.Badges {badgeName = fBadge.badge, badgeCount = fBadge.badgeCount}
