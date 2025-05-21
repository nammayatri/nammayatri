{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.ReportIssue where

import qualified Domain.Types.DriverBlockTransactions as DTDBT
import Domain.Types.Ride
import Domain.Types.ServiceTierType
import Environment
import qualified IssueManagement.Common as ICommon
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.BehaviourManagement.IssueBreach (IssueBreachType (..))
import qualified SharedLogic.BehaviourManagement.IssueBreachMitigation as IBM
import SharedLogic.DriverOnboarding
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

reportIssue :: Id Ride -> ICommon.IssueReportType -> Maybe Text -> Flow APISuccess
reportIssue rideId issueType apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  case issueType of
    ICommon.AC_RELATED_ISSUE -> do
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId ride.merchantOperatingCityId Nothing
      incrementDriverAcUsageRestrictionCount cityVehicleServiceTiers ride.driverId
    ICommon.DRIVER_TOLL_RELATED_ISSUE -> handleTollRelatedIssue ride
    ICommon.SYNC_BOOKING -> pure ()
    ICommon.EXTRA_FARE_MITIGATION -> handleExtraFareMitigation ride booking.vehicleServiceTier
    ICommon.DRUNK_AND_DRIVE_VIOLATION -> handleDrunkAndDriveViolation ride
  return Success

handleTollRelatedIssue :: Ride -> Flow ()
handleTollRelatedIssue ride = do
  driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  let tollRelatedIssueCount = fromMaybe 0 driverInfo.tollRelatedIssueCount + 1
  void $ QDI.updateTollRelatedIssueCount (Just tollRelatedIssueCount) ride.driverId

handleExtraFareMitigation :: Ride -> ServiceTierType -> Flow ()
handleExtraFareMitigation ride serviceTierType = do
  driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId (Just (DriverId (cast ride.driverId))) >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  let ibConfig = IBM.getIssueBreachConfig EXTRA_FARE_MITIGATION transporterConfig
  let allowedSTiers = ibConfig <&> (.ibAllowedServiceTiers)
  let isRideAllowedForCounting = maybe False (\allowedServiceTiers -> null allowedServiceTiers || serviceTierType `elem` allowedServiceTiers) allowedSTiers
  when isRideAllowedForCounting $
    whenJust ibConfig $ \config -> do
      QDI.updateExtraFareMitigation (pure True) ride.driverId
      IBM.incrementIssueBreachCounter EXTRA_FARE_MITIGATION ride.driverId (toInteger config.ibCountWindowSizeInDays)
      IBM.issueBreachMitigation EXTRA_FARE_MITIGATION transporterConfig driverInfo

handleDrunkAndDriveViolation :: Ride -> Flow ()
handleDrunkAndDriveViolation ride = do
  driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  let drunkAndDriveViolationCount = fromMaybe 0 driverInfo.drunkAndDriveViolationCount + 1
  person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  if drunkAndDriveViolationCount < 2
    then do
      overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory person.merchantOperatingCityId "DRUNK_AND_DRIVE_WARNING" (fromMaybe ENGLISH person.language) Nothing Nothing Nothing >>= fromMaybeM (InternalError "Overlay not found for EDIT_LOCATION")
      let fcmOverlayReq = Notify.mkOverlayReq overlay
      let entityData = Notify.DrunkAndDriveViolationWarningData {driverId = ride.driverId.getId, drunkAndDriveViolationCount}
      Notify.drunkAndDriveViolationWarningOverlay person.merchantOperatingCityId person fcmOverlayReq entityData
    else QDriverInfo.updateDynamicBlockedStateWithActivity ride.driverId (Just "DRUNK_AND_DRIVE_VIOLATION") Nothing "AUTOMATICALLY_BLOCKED_BY_APP" person.merchantId "AUTOMATICALLY_BLOCKED_BY_APP" ride.merchantOperatingCityId DTDBT.Application True Nothing Nothing DrunkAndDriveViolation
  void $ QDI.updateDrunkAndDriveViolationCount (Just drunkAndDriveViolationCount) ride.driverId
