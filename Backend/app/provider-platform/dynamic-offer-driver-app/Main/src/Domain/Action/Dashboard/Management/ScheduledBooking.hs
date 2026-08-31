{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.ScheduledBooking
  ( getScheduledBookingList,
    getScheduledBookingInfo,
    getScheduledBookingDriverDistance,
    postScheduledBookingAssign,
    postScheduledBookingUnassign,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.ScheduledBooking as Common
import qualified Domain.Action.UI.Driver as UIDriver
import Domain.Action.UI.Ride.CancelRide.Internal (cancelRideImpl)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.DriverLocation as DDL
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRideStatus
import qualified Domain.Types.RiderDetails as DRD
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps (getCoordinates)
import qualified Kernel.External.Maps.Types as KEMT
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error
import qualified Tools.Maps as Maps

-- | Include bookings whose startTime just passed, so a ride does not vanish from the ops
-- list at the exact moment of pickup (agreed 30-minute grace window).
graceWindowSeconds :: Int
graceWindowSeconds = 30 * 60

-- | Upper bound used when the caller does not pass a `to` filter.
defaultLookaheadSeconds :: Int
defaultLookaheadSeconds = 90 * 24 * 60 * 60

maxLimit :: Int
maxLimit = 20

defaultLimit :: Int
defaultLimit = 10

getScheduledBookingList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Common.AssignmentStatus ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Flow Common.ScheduledBookingListRes
getScheduledBookingList merchantShortId opCity mbAssignmentStatus mbFrom mbLimit mbOffset mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  now <- getCurrentTime
  let fromTime = fromMaybe (addUTCTime (secondsToNominalDiffTime . Seconds $ negate graceWindowSeconds) now) mbFrom
      toTime = fromMaybe (addUTCTime (secondsToNominalDiffTime $ Seconds defaultLookaheadSeconds) now) mbTo
      limit = min maxLimit $ fromMaybe defaultLimit mbLimit
      offset = fromMaybe 0 mbOffset
  let statuses = case mbAssignmentStatus of
        Just Common.ASSIGNED -> [SRB.TRIP_ASSIGNED]
        Just Common.UNASSIGNED -> [SRB.NEW]
        Nothing -> [SRB.NEW, SRB.TRIP_ASSIGNED]
  liteBookings <- QBookingLite.findScheduledUpcomingBookingsLite merchantOpCity.id statuses fromTime toTime (limit + 1) offset
  let pageRows = take limit liteBookings
      hasMorePages = length liteBookings > limit
  bookings <- forM pageRows $ \b -> do
    mbRide <- QRideLite.findActiveByRBIdLite b.id
    buildListItem b mbRide
  pure Common.ScheduledBookingListRes {totalItems = offset + length pageRows + (if hasMorePages then 1 else 0), bookings}

buildListItem :: QBookingLite.BookingLite -> Maybe QRideLite.RideLite -> Flow Common.ScheduledBookingListItem
buildListItem booking mbRide = do
  mbPickup <- resolvePickupLocation booking.id
  mbRiderPhoneNo <- fetchRiderPhone booking.riderId
  (mbDriverName, mbDriverPhoneNo) <- fetchDriverNameAndPhone (mbRide <&> (.driverId))
  txnBookings <- QBookingLite.findAllByTransactionIdLite booking.transactionId
  pure
    Common.ScheduledBookingListItem
      { transactionId = booking.transactionId,
        bookingId = booking.id.getId,
        tripCategory = booking.tripCategory,
        scheduledAt = booking.startTime,
        fromLocation = maybe emptyLocationAPIEntity mkLocationAPIEntity mbPickup,
        toLocation = Nothing, -- list shows pickup only; the detail endpoint resolves the drop
        riderName = booking.riderName,
        riderPhoneNo = mbRiderPhoneNo,
        driverName = mbDriverName,
        driverPhoneNo = mbDriverPhoneNo,
        driverId = (\r -> r.driverId.getId) <$> mbRide,
        bookingStatus = castBookingStatus booking.status,
        rideStatus = castRideStatus . (.status) <$> mbRide,
        reallocationCount = max 0 (length txnBookings - 1)
      }

-- Resolve just the pickup location for one booking (2 reads: latest pickup mapping + location).
resolvePickupLocation :: Id SRB.Booking -> Flow (Maybe DLoc.Location)
resolvePickupLocation bookingId = do
  mbMapping <- QLM.getLatestStartByEntityId bookingId.getId
  maybe (pure Nothing) (QL.findById . (.locationId)) mbMapping

getScheduledBookingInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow Common.ScheduledBookingInfoRes
getScheduledBookingInfo merchantShortId opCity transactionId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  booking <- QBooking.findByTransactionId transactionId >>= fromMaybeM (BookingNotFound transactionId)
  unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId && booking.isScheduled) $
    throwError (BookingNotFound transactionId)
  mbRide <- QRide.findActiveByRBId booking.id
  mbRiderPhoneNo <- fetchRiderPhone booking.riderId
  (mbDriverName, mbDriverPhoneNo) <- fetchDriverNameAndPhone (mbRide <&> (.driverId))
  (mbDistanceToPickup, mbEtaDuration, mbDriverLocation) <- maybe (pure (Nothing, Nothing, Nothing)) (getDriverProximityToPickup booking) mbRide
  reallocationHistory <- buildReallocationHistory booking.transactionId
  pure
    Common.ScheduledBookingInfoRes
      { transactionId = booking.transactionId,
        bookingId = booking.id.getId,
        tripCategory = booking.tripCategory,
        scheduledAt = booking.startTime,
        fromLocation = mkLocationAPIEntity booking.fromLocation,
        toLocation = mkLocationAPIEntity <$> booking.toLocation,
        riderName = booking.riderName,
        riderPhoneNo = mbRiderPhoneNo,
        driverName = mbDriverName,
        driverPhoneNo = mbDriverPhoneNo,
        driverId = (\r -> r.driverId.getId) <$> mbRide,
        bookingStatus = castBookingStatus booking.status,
        rideStatus = castRideStatus . (.status) <$> mbRide,
        driverCurrentLocation = (\loc -> KEMT.LatLong {lat = loc.lat, lon = loc.lon}) <$> mbDriverLocation,
        driverDistanceToPickup = mbDistanceToPickup,
        driverEtaToPickupSeconds = (.getSeconds) <$> mbEtaDuration,
        -- rental package (estimated distance/duration of the booked package; also present for other trips)
        estimatedDistance = booking.estimatedDistance,
        estimatedDurationSeconds = (.getSeconds) <$> booking.estimatedDuration,
        reallocationHistory
      }

-- | Reconstruct the reallocation timeline: every booking that has ever shared this
-- transactionId (bookingId changes on reallocation, transactionId stays), oldest first,
-- joined with its ride and cancellation reason.
buildReallocationHistory :: Text -> Flow [Common.ReallocationEventItem]
buildReallocationHistory transactionId = do
  txnBookings <- QBooking.findAllByTransactionId transactionId
  forM txnBookings $ \b -> do
    mbRide <- QRide.findOneByBookingId b.id
    (mbDriverName, mbDriverPhoneNo) <- fetchDriverNameAndPhone (mbRide <&> (.driverId))
    mbBCReason <- QBCReason.findByBookingId b.id
    pure
      Common.ReallocationEventItem
        { bookingId = b.id.getId,
          driverName = mbDriverName,
          driverPhoneNo = mbDriverPhoneNo,
          driverId = (\r -> r.driverId.getId) <$> mbRide,
          rideStatus = castRideStatus . (.status) <$> mbRide,
          bookingStatus = castBookingStatus b.status,
          becameCurrentAt = b.createdAt,
          cancelledAt = if b.status == SRB.CANCELLED then Just b.updatedAt else Nothing,
          cancellationSource = castCancellationSource . (.source) <$> mbBCReason,
          driverDistToPickupAtCancel = mbBCReason >>= (.driverDistToPickup)
        }

getScheduledBookingDriverDistance ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow Common.DriverDistanceRes
getScheduledBookingDriverDistance merchantShortId opCity transactionId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  booking <- QBooking.findByTransactionId transactionId >>= fromMaybeM (BookingNotFound transactionId)
  unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId && booking.isScheduled) $
    throwError (BookingNotFound transactionId)
  now <- getCurrentTime
  mbRide <- QRide.findActiveByRBId booking.id
  case mbRide of
    Nothing ->
      pure
        Common.DriverDistanceRes
          { assigned = False,
            driverId = Nothing,
            driverCurrentLocation = Nothing,
            distanceToPickupMeters = Nothing,
            etaToPickupSeconds = Nothing,
            computedAt = now
          }
    Just ride -> do
      (mbDist, mbDur, mbLoc) <- getDriverProximityToPickup booking ride
      pure
        Common.DriverDistanceRes
          { assigned = True,
            driverId = Just ride.driverId.getId,
            driverCurrentLocation = (\loc -> KEMT.LatLong {lat = loc.lat, lon = loc.lon}) <$> mbLoc,
            distanceToPickupMeters = mbDist,
            etaToPickupSeconds = (.getSeconds) <$> mbDur,
            computedAt = now
          }

postScheduledBookingAssign ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Common.AssignDriverReq ->
  Flow APISuccess.APISuccess
postScheduledBookingAssign merchantShortId opCity transactionId _mbRequestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  booking <- QBooking.findByTransactionId transactionId >>= fromMaybeM (BookingNotFound transactionId)
  unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId && booking.isScheduled) $
    throwError (BookingNotFound transactionId)
  let driverPersonId = cast req.driverId :: Id DP.Person
  -- driver's vehicle must be able to serve the booking's service tier (same check the fleet-assign uses)
  vehicle <- QVehicle.findById driverPersonId >>= fromMaybeM (VehicleNotFound driverPersonId.getId)
  unless (booking.vehicleServiceTier `elem` vehicle.selectedServiceTiers) $
    throwError (InvalidRequest "Booking's service tier is not in driver's selected service tiers")
  void $ UIDriver.acceptScheduledBooking (driverPersonId, merchant.id, merchantOpCity.id) req.clientId booking.id
  pure APISuccess.Success

postScheduledBookingUnassign ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Flow APISuccess.APISuccess
postScheduledBookingUnassign merchantShortId opCity transactionId _mbRequestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  booking <- QBooking.findByTransactionId transactionId >>= fromMaybeM (BookingNotFound transactionId)
  unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId && booking.isScheduled) $
    throwError (BookingNotFound transactionId)
  ride <- QRide.findActiveByRBId booking.id >>= fromMaybeM (InvalidRequest "No driver assigned to this booking")
  let bookingCReason =
        DBCReason.BookingCancellationReason
          { driverId = Just ride.driverId,
            bookingId = booking.id,
            rideId = Just ride.id,
            merchantId = Just merchant.id,
            source = DBCReason.ByMerchant,
            reasonCode = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            distanceUnit = booking.distanceUnit,
            merchantOperatingCityId = Just booking.merchantOperatingCityId
          }
  cancelRideImpl ride.id DRide.Dashboard bookingCReason True Nothing False
  pure APISuccess.Success

-- Live driver proximity to pickup via OSRM (Maps.getDistance -> .getDistances config): one LTS
-- location fetch + one distance call that returns both distance and duration.
getDriverProximityToPickup :: SRB.Booking -> DRide.Ride -> Flow (Maybe Meters, Maybe Seconds, Maybe DDL.DriverLocation)
getDriverProximityToPickup booking ride = do
  mbDriver <- QPerson.findById ride.driverId
  driverLocations <- LF.driversLocationByCloudType [ride.driverId] (mbDriver >>= (.cloudType))
  case listToMaybe driverLocations of
    Nothing -> pure (Nothing, Nothing, Nothing)
    Just loc -> do
      distRes <-
        Maps.getDistance booking.providerId booking.merchantOperatingCityId (Just booking.id.getId) $
          Maps.GetDistanceReq
            { origin = getCoordinates loc,
              destination = getCoordinates booking.fromLocation,
              travelMode = Just Maps.CAR,
              distanceUnit = booking.distanceUnit,
              sourceDestinationMapping = Nothing
            }
      pure (Just distRes.distance, Just distRes.duration, Just loc)

-- Helpers -----------------------------------------------------------------

fetchRiderPhone :: Maybe (Id DRD.RiderDetails) -> Flow (Maybe Text)
fetchRiderPhone Nothing = pure Nothing
fetchRiderPhone (Just riderId) = do
  mbRiderDetails <- QRiderDetails.findById riderId
  case mbRiderDetails of
    Nothing -> pure Nothing
    Just riderDetails -> Just <$> decrypt riderDetails.mobileNumber

fetchDriverNameAndPhone :: Maybe (Id DP.Person) -> Flow (Maybe Text, Maybe Text)
fetchDriverNameAndPhone Nothing = pure (Nothing, Nothing)
fetchDriverNameAndPhone (Just driverId) = do
  mbPerson <- QPerson.findById driverId
  case mbPerson of
    Nothing -> pure (Nothing, Nothing)
    Just person -> do
      phone <- mapM decrypt person.mobileNumber
      pure (Just (personName person), phone)

-- Placeholder used only if a booking's pickup mapping is somehow missing (should not happen
-- for a valid booking); keeps the list resilient instead of failing the whole request.
emptyLocationAPIEntity :: Common.LocationAPIEntity
emptyLocationAPIEntity =
  Common.LocationAPIEntity
    { lat = 0,
      lon = 0,
      street = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      areaCode = Nothing,
      area = Nothing,
      fullAddress = Nothing
    }

personName :: DP.Person -> Text
personName person = person.firstName <> maybe "" (" " <>) person.lastName

mkLocationAPIEntity :: DLoc.Location -> Common.LocationAPIEntity
mkLocationAPIEntity loc =
  let DLoc.LocationAddress {..} = loc.address
   in Common.LocationAPIEntity
        { lat = loc.lat,
          lon = loc.lon,
          street = street,
          city = city,
          state = state,
          country = country,
          building = building,
          areaCode = areaCode,
          area = area,
          fullAddress = fullAddress
        }

castBookingStatus :: SRB.BookingStatus -> Common.BookingStatus
castBookingStatus = \case
  SRB.NEW -> Common.NEW
  SRB.TRIP_ASSIGNED -> Common.TRIP_ASSIGNED
  SRB.COMPLETED -> Common.COMPLETED
  SRB.CANCELLED -> Common.CANCELLED
  SRB.REALLOCATED -> Common.REALLOCATED

-- driver-app keeps its own Domain.Types.Ride.RideStatus; the API contract uses the shared
-- beckn-spec Domain.Types.RideStatus. Same constructors, so this is a pure boundary cast.
castRideStatus :: DRide.RideStatus -> DRideStatus.RideStatus
castRideStatus = \case
  DRide.UPCOMING -> DRideStatus.UPCOMING
  DRide.NEW -> DRideStatus.NEW
  DRide.INPROGRESS -> DRideStatus.INPROGRESS
  DRide.COMPLETED -> DRideStatus.COMPLETED
  DRide.CANCELLED -> DRideStatus.CANCELLED

castCancellationSource :: DBCReason.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCReason.ByUser -> Common.ByUser
  DBCReason.ByDriver -> Common.ByDriver
  DBCReason.ByMerchant -> Common.ByMerchant
  DBCReason.ByAllocator -> Common.ByAllocator
  DBCReason.ByApplication -> Common.ByApplication
  DBCReason.ByFleetOwner -> Common.ByFleetOwner
