{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Volunteer where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Volunteer as Common
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.StartRide as RideStart
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Booking.BookingLocation as Domain
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.Person (findPerson)
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

bookingInfo :: ShortId DM.Merchant -> Text -> Flow Common.BookingInfoResponse
bookingInfo merchantShortId otpCode = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  -- booking <- runInReplica $ QBooking.findBookingBySpecialZoneOTP merchant.id otpCode now >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp otpCode)
  booking <- QBooking.findBookingBySpecialZoneOTP merchant.id otpCode now >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp otpCode)
  return $ buildMessageInfoResponse booking
  where
    buildMessageInfoResponse Domain.Booking {..} =
      Common.BookingInfoResponse
        { bookingId = cast id,
          fromLocation = buildBookingLocation fromLocation,
          toLocation = buildBookingLocation toLocation,
          estimatedDistance,
          estimatedFare,
          estimatedDuration,
          riderName
        }
    buildBookingLocation Domain.BookingLocation {..} =
      Common.BookingLocation
        { address = buildLocationAddress address,
          id = cast id,
          ..
        }

    buildLocationAddress Domain.LocationAddress {..} =
      Common.LocationAddress
        { ..
        }

assignCreateAndStartOtpRide :: ShortId DM.Merchant -> Common.AssignCreateAndStartOtpRideAPIReq -> Flow APISuccess
assignCreateAndStartOtpRide _ Common.AssignCreateAndStartOtpRideAPIReq {..} = do
  requestor <- findPerson (cast driverId)
  -- booking <- runInReplica $ QBooking.findById (cast bookingId) >>= fromMaybeM (BookingNotFound bookingId.getId)
  booking <- QBooking.findById (cast bookingId) >>= fromMaybeM (BookingNotFound bookingId.getId)
  rideOtp <- booking.specialZoneOtpCode & fromMaybeM (InternalError "otpCode not found for special zone booking")

  ride <- DRide.otpRideCreate requestor rideOtp booking
  let driverReq = RideStart.DriverStartRideReq {rideOtp, point, requestor}
  shandle <- RideStart.buildStartRideHandle requestor.merchantId
  void $ RideStart.driverStartRide shandle ride.id driverReq
  return Success
