{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.ReportIssue where

import Domain.Types.Ride
import Environment
import qualified IssueManagement.Common as ICommon
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Error
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Ride as QRide
import Tools.Error

reportIssue :: Id Ride -> ICommon.IssueReportType -> Maybe Text -> Flow APISuccess
reportIssue rideId issueType apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  case issueType of
    ICommon.AC_RELATED_ISSUE -> do
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId ride.merchantOperatingCityId
      incrementDriverAcUsageRestrictionCount cityVehicleServiceTiers ride.driverId
    ICommon.DRIVER_TOLL_RELATED_ISSUE -> handleTollRelatedIssue ride
  return Success

handleTollRelatedIssue :: Ride -> Flow ()
handleTollRelatedIssue ride = do
  driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  let tollRelatedIssueCount = fromMaybe 0 driverInfo.tollRelatedIssueCount + 1
  void $ QDI.updateTollRelatedIssueCount (Just tollRelatedIssueCount) ride.driverId
