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
    rideList,
    rideInfo,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import Data.Coerce (coerce)
import Domain.Types.Booking.BookingLocation (BookingLocation (..))
import qualified Domain.Types.Booking.Type as DB
import qualified Domain.Types.BookingCancellationReason as DBCReason
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (count, isNothing)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.CachedQueries.Merchant (findByShortId)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide

---------------------------------------------------------------------

mkCommonRideStatus :: DRide.RideStatus -> Common.RideStatus
mkCommonRideStatus rs = case rs of
  DRide.NEW -> Common.NEW
  DRide.INPROGRESS -> Common.INPROGRESS
  DRide.COMPLETED -> Common.COMPLETED
  DRide.CANCELLED -> Common.CANCELLED

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
    DRide.COMPLETED -> throwError $ RideInvalidStatus "This ride is completed"
    DRide.CANCELLED -> throwError $ RideInvalidStatus "This ride is cancelled"
    _ -> pure ()
  person <- runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  let mbtoLocation = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ mkCommonBookingLocation locationDetail.toLocation
        DB.DriverOfferDetails driverOfferDetail -> Just $ mkCommonBookingLocation driverOfferDetail.toLocation
        _ -> Nothing
  let mbDistance = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ locationDetail.distance
        DB.DriverOfferDetails driverOfferDetail -> Just $ driverOfferDetail.distance
        DB.OneWaySpecialZoneDetails oneWaySpecialZoneDetail -> Just $ oneWaySpecialZoneDetail.distance
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
        estimatedDistance = mbDistance,
        rideStartTime = ride.rideStartTime,
        rideEndTime = ride.rideEndTime,
        userFirstName = person.firstName,
        userLastName = person.lastName,
        fromLocation = mkCommonBookingLocation booking.fromLocation,
        toLocation = mbtoLocation
      }

---------------------------------------------------------------------
rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbReqShortRideId mbCustomerPhone mbDriverPhone = do
  merchant <- findMerchantByShortId merchantShortId
  let limit_ = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset_ = fromMaybe 0 mbOffset
  let mbShortRideId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbReqShortRideId
  mbCustomerPhoneDBHash <- getDbHash `traverse` mbCustomerPhone
  now <- getCurrentTime
  rideItems <- runInReplica $ QRide.findAllRideItems merchant.id limit_ offset_ mbBookingStatus mbShortRideId mbCustomerPhoneDBHash mbDriverPhone now
  rideListItems <- traverse buildRideListItem rideItems
  let count = length rideListItems
  -- should we consider filters in totalCount, e.g. count all canceled rides?
  -- totalCount <- runInReplica $ QRide.countRides merchant.id
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.RideListRes {totalItems = count, summary, rides = rideListItems}
  where
    maxLimit = 20
    defaultLimit = 10

buildRideListItem :: EncFlow m r => QRide.RideItem -> m Common.RideListItem
buildRideListItem QRide.RideItem {..} = do
  customerPhoneNo <- mapM decrypt person.mobileNumber
  pure
    Common.RideListItem
      { rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) ride.shortId,
        rideCreatedAt = ride.createdAt,
        rideId = cast @DRide.Ride @Common.Ride ride.id,
        customerName = person.firstName,
        customerPhoneNo,
        driverName = ride.driverName,
        driverPhoneNo = ride.driverMobileNumber,
        vehicleNo = ride.vehicleNumber,
        bookingStatus
      }

rideInfo :: ShortId DM.Merchant -> Id Common.Ride -> Flow Common.RideInfoRes
rideInfo merchantShortId reqRideId = do
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  merchant <- findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (merchant.id == booking.merchantId) $ throwError (RideDoesNotExist rideId.getId)
  person <- runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  mbBCReason <-
    if ride.status == DRide.CANCELLED
      then runInReplica $ QBCReason.findByRideBookingId booking.id
      else pure Nothing
  let cancelledTime = case ride.status of
        DRide.CANCELLED -> Just ride.updatedAt
        _ -> Nothing
  let mbtoLocation = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ mkCommonBookingLocation locationDetail.toLocation
        DB.DriverOfferDetails driverOfferDetail -> Just $ mkCommonBookingLocation driverOfferDetail.toLocation
        _ -> Nothing
  let cancelledBy = castCancellationSource <$> (mbBCReason <&> (.source))
  pure
    Common.RideInfoRes
      { rideId = reqRideId,
        bookingId = cast ride.bookingId,
        rideStatus = mkCommonRideStatus ride.status,
        customerName = person.firstName,
        customerPhoneNo = person.unencryptedMobileNumber,
        rideOtp = ride.otp,
        fromLocation = mkCommonBookingLocation booking.fromLocation,
        toLocation = mbtoLocation,
        driverName = ride.driverName,
        driverPhoneNo = Just ride.driverMobileNumber,
        driverRegisteredAt = ride.driverRegisteredAt,
        vehicleNumber = ride.vehicleNumber,
        vehicleModel = ride.vehicleModel,
        rideBookingTime = booking.createdAt,
        driverArrivalTime = ride.driverArrivalTime,
        rideStartTime = ride.rideStartTime,
        rideEndTime = ride.rideEndTime,
        chargeableDistance = ride.chargeableDistance,
        estimatedFare = booking.estimatedFare,
        actualFare = ride.fare,
        rideDuration = timeDiffInMinutes <$> ride.rideEndTime <*> ride.rideStartTime,
        cancelledTime = cancelledTime,
        cancelledBy = cancelledBy
      }

timeDiffInMinutes :: UTCTime -> UTCTime -> Minutes
timeDiffInMinutes t1 = secondsToMinutes . nominalDiffTimeToSeconds . diffUTCTime t1

castCancellationSource :: DBCReason.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCReason.ByUser -> Common.ByUser
  DBCReason.ByDriver -> Common.ByDriver
  DBCReason.ByMerchant -> Common.ByMerchant
  DBCReason.ByAllocator -> Common.ByAllocator
  DBCReason.ByApplication -> Common.ByApplication
