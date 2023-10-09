{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Volunteer where

import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Volunteer as Common
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.StartRide as RideStart
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Booking.BookingLocation as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle.Variant as Domain
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Common (Forkable (fork), MonadTime (getCurrentTime))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.Person (findPerson)
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.SMS as Sms

bookingInfo :: ShortId DM.Merchant -> Text -> Flow Common.BookingInfoResponse
bookingInfo merchantShortId otpCode = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  transporterConfig <- TC.findByMerchantId merchant.id >>= fromMaybeM (TransporterConfigNotFound merchant.id.getId)
  booking <- runInReplica $ QBooking.findBookingBySpecialZoneOTP merchant.id otpCode now transporterConfig.specialZoneBookingOtpExpiry >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp otpCode)
  return $ buildMessageInfoResponse booking
  where
    buildMessageInfoResponse Domain.Booking {..} =
      Common.BookingInfoResponse
        { bookingId = cast id,
          fromLocation = buildBookingLocation fromLocation,
          toLocation = buildBookingLocation <$> toLocation,
          estimatedDistance,
          estimatedFare,
          estimatedDuration,
          riderName,
          vehicleVariant = convertVehicleVariant vehicleVariant
        }

    convertVehicleVariant Domain.SEDAN = Common.SEDAN
    convertVehicleVariant Domain.SUV = Common.SUV
    convertVehicleVariant Domain.HATCHBACK = Common.HATCHBACK
    convertVehicleVariant Domain.AUTO_RICKSHAW = Common.AUTO_RICKSHAW
    convertVehicleVariant Domain.TAXI = Common.TAXI
    convertVehicleVariant Domain.TAXI_PLUS = Common.TAXI_PLUS

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
  booking <- runInReplica $ QBooking.findById (cast bookingId) >>= fromMaybeM (BookingNotFound bookingId.getId)
  rideOtp <- booking.specialZoneOtpCode & fromMaybeM (InternalError "otpCode not found for special zone booking")

  ride <- DRide.otpRideCreate requestor rideOtp booking
  let driverReq = RideStart.DriverStartRideReq {rideOtp, point, requestor}
  fork "sending dashboard sms - start ride" $ do
    mride <- runInReplica $ QRide.findById ride.id >>= fromMaybeM (RideDoesNotExist ride.id.getId)
    Sms.sendDashboardSms booking.providerId Sms.BOOKING (Just mride) mride.driverId (Just booking) 0
  shandle <- RideStart.buildStartRideHandle requestor.merchantId
  void $ RideStart.driverStartRide shandle ride.id driverReq
  return Success
