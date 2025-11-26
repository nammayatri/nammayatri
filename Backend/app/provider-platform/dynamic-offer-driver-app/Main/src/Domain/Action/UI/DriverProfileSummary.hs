{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.UI.DriverProfileSummary where

import qualified Domain.Action.UI.Person as SP
import qualified Domain.Types.DriverInformation as DriverInfo
import Domain.Types.FeedbackBadge (FeedbackBadge)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DR
import Domain.Types.Vehicle (VehicleAPIEntity)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.BehaviourManagement.CancellationRate as CR
import qualified SharedLogic.DriverOnboarding as SD
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as BQ
import qualified Storage.Queries.BookingCancellationReason as QBCR
import Storage.Queries.DriverStats
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as FPQ
import qualified Storage.Queries.FareParameters.FareParametersProgressiveDetails as FPPDQ
import qualified Storage.Queries.FeedbackBadge as QFB
import qualified Storage.Queries.FleetDriverAssociation as FDV
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
    totalDistanceTravelledWithUnit :: Distance,
    rating :: Maybe Centesimal,
    totalUsersRated :: Int,
    language :: Maybe Maps.Language,
    alternateNumber :: Maybe Text,
    gender :: Maybe SP.Gender,
    driverSummary :: DriverInfo.DriverSummary,
    missedOpp :: DriverInfo.DriverMissedOpp,
    feedbackBadges :: DriverInfo.DriverBadges,
    languagesSpoken :: Maybe [Text],
    hometown :: Maybe Text,
    cancellationRateInWindow :: Maybe Int,
    cancelledRidesCountInWindow :: Maybe Int,
    assignedRidesCountInWindow :: Maybe Int,
    fleetOwnerMobileNumber :: Maybe Text,
    fleetOwnerName :: Maybe Text,
    windowSize :: Maybe Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

getDriverProfileSummary :: (CacheFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> m DriverProfleSummaryRes
getDriverProfileSummary (driverId, _, mocId) fleetInfo = do
  person <- B.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  vehicleMB <- B.runInReplica $ QVehicle.findById person.id
  (fleetOwnerMobileNumber, fleetOwnerName) <- case fleetInfo of
    Just True -> do
      mbFda <- FDV.findByDriverId driverId True
      case mbFda of
        Nothing -> return (Nothing, Nothing)
        Just fda -> do
          fleetOwner <- B.runInReplica $ QPerson.findById (Id fda.fleetOwnerId) >>= fromMaybeM (PersonNotFound $ fda.fleetOwnerId)
          fleetOwnerMobileNumber_ <- mapM decrypt fleetOwner.mobileNumber
          return (fleetOwnerMobileNumber_, Just $ fleetOwner.firstName <> maybe "" (" " <>) fleetOwner.middleName <> maybe "" (" " <>) fleetOwner.lastName)
    _ -> return (Nothing, Nothing)

  decMobNum <- mapM decrypt person.mobileNumber
  decaltMobNum <- mapM decrypt person.alternateMobileNumber
  driverStats_ <- B.runInReplica $ QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  driverStats <-
    if driverStats_.totalEarnings == 0.0 && driverStats_.bonusEarned == 0.0 && driverStats_.lateNightTrips == 0 && driverStats_.earningsMissed == 0.0
      then do
        allRides <- B.runInReplica $ RQ.findAllRidesByDriverId driverId
        let completedRides = filter ((== DR.COMPLETED) . (.status)) allRides
            farePramIds = mapMaybe (.fareParametersId) completedRides
        lateNightTripsCount <- B.runInReplica $ FPQ.findAllLateNightRides farePramIds
        cancelledBookingIdsByDriver <- B.runInReplica $ QBCR.findAllBookingIdsCancelledByDriverId (cast person.id)
        missedEarnings <- B.runInReplica $ BQ.findFareForCancelledBookings cancelledBookingIdsByDriver
        driverSelectedFare <- B.runInReplica $ FPQ.findDriverSelectedFareEarnings farePramIds
        customerExtraFee <- B.runInReplica $ FPQ.findCustomerExtraFees farePramIds
        deadKmFare <- B.runInReplica $ FPPDQ.findDeadKmFareEarnings farePramIds
        let bonusEarnings = driverSelectedFare + customerExtraFee + deadKmFare
            totalEarnings = sum $ map (fromMaybe 0 . (.fare)) completedRides
        incrementTotalEarningsAndBonusEarnedAndLateNightTrip (cast person.id) totalEarnings bonusEarnings lateNightTripsCount
        setMissedEarnings (cast person.id) missedEarnings
        QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
      else QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  feedbackBadgeList <- B.runInReplica $ QFB.findByDriverId person.id
  totalUsersRated <- B.runInReplica $ QRating.findAllRatingUsersCountByPerson driverId

  let driverSummary =
        DriverInfo.DriverSummary
          { totalEarnings = roundToIntegral driverStats.totalEarnings,
            totalEarningsWithCurrency = PriceAPIEntity driverStats.totalEarnings driverStats.currency,
            bonusEarned = roundToIntegral driverStats.bonusEarned,
            bonusEarnedWithCurrency = PriceAPIEntity driverStats.bonusEarned driverStats.currency,
            totalCompletedTrips = driverStats.totalRides,
            lateNightTrips = driverStats.lateNightTrips,
            lastRegistered = person.createdAt
          }
  let missedOpp =
        DriverInfo.DriverMissedOpp
          { cancellationRate = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int),
            ridesCancelled = fromMaybe 0 driverStats.ridesCancelled,
            totalRides = driverStats.totalRides,
            missedEarnings = roundToIntegral driverStats.earningsMissed,
            missedEarningsWithCurrency = PriceAPIEntity driverStats.earningsMissed driverStats.currency
          }
  mbDefaultServiceTier <-
    case vehicleMB of
      Nothing -> return Nothing
      Just vehicle -> do
        cityServiceTiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId (Just [])
        return ((.serviceTierType) <$> (find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers))
  cancellationRateData <- CR.getCancellationRateData mocId driverId
  return $
    DriverProfleSummaryRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        totalRidesAssigned = fromMaybe 0 driverStats.totalRidesAssigned,
        mobileNumber = decMobNum,
        rating = SP.roundToOneDecimal <$> driverStats.rating,
        linkedVehicle = SD.makeVehicleAPIEntity mbDefaultServiceTier <$> vehicleMB,
        totalDistanceTravelled = driverStats.totalDistance,
        totalDistanceTravelledWithUnit = convertMetersToDistance driverStats.distanceUnit driverStats.totalDistance,
        language = person.language,
        alternateNumber = decaltMobNum,
        gender = Just person.gender,
        feedbackBadges = generateFeedbackBadge feedbackBadgeList,
        languagesSpoken = person.languagesSpoken,
        hometown = person.hometown,
        cancellationRateInWindow = (.cancellationRate) <$> cancellationRateData,
        cancelledRidesCountInWindow = (.cancelledCount) <$> cancellationRateData,
        assignedRidesCountInWindow = (.assignedCount) <$> cancellationRateData,
        windowSize = (.windowSize) <$> cancellationRateData,
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
