module Domain.Action.Dashboard.Ride
  ( rideList,
    rideInfo,
  )
where

import Beckn.External.Encryption (decrypt)
import Beckn.External.Maps.HasCoordinates
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Ride as Common
import Data.Coerce (coerce)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import SharedLogic.Transporter (findMerchantByShortId)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Error

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
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset = fromMaybe 0 mbOffset
  let mbShortRideId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbReqShortRideId
  rideItems <- QRide.findAllRideItems merchant.id limit offset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone
  rideListItems <- traverse buildRideListItem rideItems
  pure
    Common.RideListRes
      { totalItems = length rideListItems,
        rides = rideListItems
      }
  where
    maxLimit = 20
    defaultLimit = 10

buildRideListItem :: EncFlow m r => QRide.RideItem -> m Common.RideListItem
buildRideListItem QRide.RideItem {..} = do
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  pure
    Common.RideListItem
      { rideId = cast @DRide.Ride @Common.Ride rideDetails.id,
        rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) rideShortId,
        customerName,
        customerPhoneNo,
        driverName = rideDetails.driverName,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        bookingStatus
      }

---------------------------------------------------------------------
rideInfo :: ShortId DM.Merchant -> Id Common.Ride -> Flow Common.RideInfoRes
rideInfo merchantShortId reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideDetails <- QRideDetails.findById rideId >>= fromMaybeM (RideNotFound rideId.getId) -- FIXME RideDetailsNotFound
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound rideId.getId)
  let driverId = ride.driverId

  -- merchant access checking
  unless (merchant.id == booking.providerId) $ throwError (RideDoesNotExist rideId.getId)

  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  riderDetails <- QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound rideId.getId)
  driverLocation <- QDrLoc.findById driverId >>= fromMaybeM LocationNotFound

  mbBCReason <-
    if ride.status == DRide.CANCELLED
      then QBCReason.findByRideId rideId -- it can be Nothing if cancelled by user
      else pure Nothing
  driverInitiatedCallCount <- QCallStatus.countCallsByRideId rideId
  let cancellationReason =
        (coerce @DCReason.CancellationReasonCode @Common.CancellationReasonCode <$>) . join $ mbBCReason <&> (.reasonCode)
  let cancelledBy = castCancellationSource <$> (mbBCReason <&> (.source))

  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  now <- getCurrentTime

  let (mbToLocation, mbEstimatedDistance) = case booking.bookingDetails of
        DBooking.OneWayDetails details -> (Just details.toLocation, Just details.estimatedDistance)
        DBooking.RentalDetails _ -> (Nothing, Nothing)
  pure
    Common.RideInfoRes
      { rideId = cast @DRide.Ride @Common.Ride ride.id,
        customerName = booking.riderName,
        customerPhoneNo,
        rideOtp = ride.otp,
        customerPickupLocation = mkLocationAPIEntity booking.fromLocation,
        customerDropLocation = mkLocationAPIEntity <$> mbToLocation,
        driverId = cast @DP.Person @Common.Driver driverId,
        driverName = rideDetails.driverName,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        driverStartLocation = ride.tripStartPos,
        driverCurrentLocation = getCoordinates driverLocation,
        rideBookingTime = booking.createdAt,
        rideStartTime = ride.tripStartTime,
        rideEndTime = ride.tripEndTime,
        rideDistanceEstimated = mbEstimatedDistance,
        rideDistanceActual = roundToIntegral ride.traveledDistance,
        pickupDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just ride.createdAt),
        rideDuration = timeDiffInMinutes <$> ride.tripEndTime <*> ride.tripStartTime,
        bookingStatus = mkBookingStatus ride now,
        cancelledBy,
        cancellationReason,
        driverInitiatedCallCount,
        bookingToRideStartDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just booking.createdAt)
      }

mkLocationAPIEntity :: DBLoc.BookingLocation -> Common.LocationAPIEntity
mkLocationAPIEntity DBLoc.BookingLocation {..} = do
  let DBLoc.LocationAddress {..} = address
  Common.LocationAPIEntity {..}

castCancellationSource :: DBCReason.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCReason.ByUser -> Common.ByUser
  DBCReason.ByDriver -> Common.ByDriver
  DBCReason.ByMerchant -> Common.ByMerchant
  DBCReason.ByAllocator -> Common.ByAllocator

timeDiffInMinutes :: UTCTime -> UTCTime -> Minutes
timeDiffInMinutes t1 = secondsToMinutes . nominalDiffTimeToSeconds . diffUTCTime t1

-- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.tripStartTime = Nothing
mkBookingStatus :: DRide.Ride -> UTCTime -> Common.BookingStatus
mkBookingStatus ride now = do
  let sixHours = secondsToNominalDiffTime $ Seconds 21600
  let ongoing6HrsCond = maybe True (\tripStartTime -> diffUTCTime now tripStartTime >= sixHours) ride.tripStartTime
  case ride.status of
    DRide.NEW -> Common.UPCOMING
    DRide.INPROGRESS | not ongoing6HrsCond -> Common.ONGOING
    DRide.INPROGRESS -> Common.ONGOING_6HRS
    DRide.COMPLETED -> Common.COMPLETED
    DRide.CANCELLED -> Common.CANCELLED
