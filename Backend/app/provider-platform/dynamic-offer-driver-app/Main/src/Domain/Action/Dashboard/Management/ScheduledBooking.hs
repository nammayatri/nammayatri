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
    postScheduledBookingOpsNoteAdd,
    putScheduledBookingOpsNoteUpdate,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.ScheduledBooking as Common
import Data.List (sortOn)
import Domain.Action.UI.Ride.CancelRide.Internal (getDistanceToPickup)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import Domain.Types.OpsNote (OpsNoteStatus (..))
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRideStatus
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.ScheduledBookingOpsNote as DOpsNote
import Environment
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Maps.Types as KEMT
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
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
import qualified Storage.Queries.ScheduledBookingOpsNote as QOpsNote
import Tools.Error

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
  -- Lite read for the whole matching set: scalar columns only, NO LocationMapping/Location
  -- joins. Pickup is resolved per-row for the current page only (buildListItem); the detail
  -- endpoint resolves full pickup + drop for the single opened booking.
  liteBookings <- QBookingLite.findScheduledUpcomingBookingsLite merchantOpCity.id [SRB.NEW, SRB.TRIP_ASSIGNED] fromTime toTime
  -- A scheduled booking stays status = NEW even after a driver accepts: the ride is created
  -- UPCOMING and the booking flips to TRIP_ASSIGNED only at the scheduled dispatch near pickup.
  -- So "assigned" is derived from an active (non-cancelled) ride; RideLite avoids ride-location joins.
  withRides <- forM liteBookings $ \b -> do
    mbRide <- QRideLite.findActiveByRBIdLite b.id
    pure (b, mbRide)
  let matchesAssignment (_, mbRide) = case mbAssignmentStatus of
        Just Common.ASSIGNED -> isJust mbRide
        Just Common.UNASSIGNED -> isNothing mbRide
        Nothing -> True
      filtered = sortOn ((.startTime) . fst) $ filter matchesAssignment withRides
      page = take limit $ drop offset filtered
  bookings <- mapM (uncurry buildListItem) page
  pure Common.ScheduledBookingListRes {totalItems = length filtered, bookings}

buildListItem :: QBookingLite.BookingLite -> Maybe QRideLite.RideLite -> Flow Common.ScheduledBookingListItem
buildListItem booking mbRide = do
  mbPickup <- resolvePickupLocation booking.id
  mbRiderPhoneNo <- fetchRiderPhone booking.riderId
  (mbDriverName, mbDriverPhoneNo) <- fetchDriverNameAndPhone (mbRide <&> (.driverId))
  txnBookings <- QBookingLite.findAllByTransactionIdLite booking.transactionId
  opsNotes <- QOpsNote.findByTransactionId booking.transactionId
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
        bookingStatus = castBookingStatus booking.status,
        rideStatus = castRideStatus . (.status) <$> mbRide,
        reallocationCount = max 0 (length txnBookings - 1),
        hasPendingOpsFlags = any ((== PENDING) . (.status)) opsNotes
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
  unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId) $
    throwError (BookingNotFound transactionId)
  mbRide <- QRide.findActiveByRBId booking.id
  mbRiderPhoneNo <- fetchRiderPhone booking.riderId
  (mbDriverName, mbDriverPhoneNo) <- fetchDriverNameAndPhone (mbRide <&> (.driverId))
  (mbDistanceToPickup, mbDriverLocation) <- getDistanceToPickup booking mbRide
  opsNotes <- QOpsNote.findByTransactionId transactionId
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
        bookingStatus = castBookingStatus booking.status,
        rideStatus = castRideStatus . (.status) <$> mbRide,
        driverCurrentLocation = (\loc -> KEMT.LatLong {lat = loc.lat, lon = loc.lon}) <$> mbDriverLocation,
        driverDistanceToPickup = mbDistanceToPickup,
        driverEtaToPickupSeconds = (.getSeconds) <$> booking.dqDurationToPickup,
        -- rental package (estimated distance/duration of the booked package; also present for other trips)
        estimatedDistance = booking.estimatedDistance,
        estimatedDurationSeconds = (.getSeconds) <$> booking.estimatedDuration,
        opsNotes = map mkOpsNoteItem opsNotes,
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
          rideStatus = castRideStatus . (.status) <$> mbRide,
          bookingStatus = castBookingStatus b.status,
          becameCurrentAt = b.createdAt,
          cancelledAt = if b.status == SRB.CANCELLED then Just b.updatedAt else Nothing,
          cancellationSource = castCancellationSource . (.source) <$> mbBCReason,
          driverDistToPickupAtCancel = mbBCReason >>= (.driverDistToPickup)
        }

postScheduledBookingOpsNoteAdd ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Common.AddOpsNoteReq ->
  Flow APISuccess.APISuccess
postScheduledBookingOpsNoteAdd merchantShortId opCity transactionId mbRequestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  -- ops notes are only allowed where a booking exists for the transactionId
  booking <- QBooking.findByTransactionId transactionId >>= fromMaybeM (BookingNotFound transactionId)
  unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId) $
    throwError (BookingNotFound transactionId)
  noteId <- generateGUID
  now <- getCurrentTime
  QOpsNote.create
    DOpsNote.ScheduledBookingOpsNote
      { id = noteId,
        transactionId = transactionId,
        bookingId = Just booking.id,
        noteType = req.noteType,
        content = req.content,
        status = fromMaybe PENDING req.status,
        createdByDashboardUserId = mbRequestorId,
        merchantId = Just merchant.id,
        merchantOperatingCityId = Just merchantOpCity.id,
        createdAt = now,
        updatedAt = now
      }
  pure APISuccess.Success

putScheduledBookingOpsNoteUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Common.UpdateOpsNoteReq ->
  Flow APISuccess.APISuccess
putScheduledBookingOpsNoteUpdate _merchantShortId _opCity noteId _mbRequestorId req = do
  note <- QOpsNote.findById (Id noteId) >>= fromMaybeM (InvalidRequest $ "Ops note not found: " <> noteId)
  let newStatus = fromMaybe note.status req.status
      newContent = maybe note.content Just req.content
  QOpsNote.updateNote newStatus newContent (Id noteId)
  pure APISuccess.Success

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

mkOpsNoteItem :: DOpsNote.ScheduledBookingOpsNote -> Common.OpsNoteItem
mkOpsNoteItem note =
  Common.OpsNoteItem
    { id = note.id.getId,
      noteType = note.noteType,
      content = note.content,
      status = note.status,
      bookingId = (.getId) <$> note.bookingId,
      createdByDashboardUserId = note.createdByDashboardUserId,
      createdAt = note.createdAt,
      updatedAt = note.updatedAt
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
