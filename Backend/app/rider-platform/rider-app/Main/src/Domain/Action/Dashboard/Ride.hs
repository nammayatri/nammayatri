{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Ride
  ( shareRideInfo,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import Domain.Types.Booking.BookingLocation (BookingLocation (..))
import qualified Domain.Types.Booking.Type as DB
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as Domain
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.CachedQueries.Merchant (findByShortId)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide

---------------------------------------------------------------------

mkCommonRideStatus :: Domain.RideStatus -> Common.RideStatus
mkCommonRideStatus rs = case rs of
  Domain.NEW -> Common.NEW
  Domain.INPROGRESS -> Common.INPROGRESS
  Domain.COMPLETED -> Common.COMPLETED
  Domain.CANCELLED -> Common.CANCELLED

mkCommonBookingLocation :: BookingLocation -> Common.BookingLocation
mkCommonBookingLocation BookingLocation {..} =
  Common.BookingLocation
    { id = cast @BookingLocation @Common.BookingLocation id,
      address = mkAddressRes address,
      ..
    }

mkAddressRes :: LocationAddress -> Common.LocationAddress
mkAddressRes LocationAddress {..} = Common.LocationAddress {..}

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  Flow Common.ShareRideInfoRes
shareRideInfo merchantId rideId = do
  ride <- runInReplica $ QRide.findById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  merchant <- findByShortId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  unless (merchant.id == booking.merchantId) $ throwError (RideDoesNotExist rideId.getId)
  case ride.status of
    Domain.COMPLETED -> throwError $ RideInvalidStatus "This ride is completed"
    Domain.CANCELLED -> throwError $ RideInvalidStatus "This ride is cancelled"
    _ -> pure ()
  person <- runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  let mbtoLocation = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ mkCommonBookingLocation locationDetail.toLocation
        DB.DriverOfferDetails driverOfferDetail -> Just $ mkCommonBookingLocation driverOfferDetail.toLocation
        _ -> Nothing
  return $
    Common.ShareRideInfoRes
      { id = cast ride.id,
        bookingId = cast ride.bookingId,
        status = mkCommonRideStatus ride.status,
        driverName = ride.driverName,
        driverRating = ride.driverRating,
        vehicleNumber = ride.vehicleNumber,
        vehicleModel = ride.vehicleModel,
        vehicleColor = ride.vehicleColor,
        trackingUrl = ride.trackingUrl,
        rideStartTime = ride.rideStartTime,
        rideEndTime = ride.rideEndTime,
        userFirstName = person.firstName,
        userLastName = person.lastName,
        fromLocation = mkCommonBookingLocation booking.fromLocation,
        toLocation = mbtoLocation
      }
