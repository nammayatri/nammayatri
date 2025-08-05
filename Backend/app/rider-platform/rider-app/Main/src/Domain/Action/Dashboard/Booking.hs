{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Booking
  ( postBookingCancelAllStuck,
    postBookingSyncMultiple,
  )
where

import Beckn.ACL.Status
import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import Data.Coerce (coerce)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.BookingStatus as DBooking
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Ride as QRide
import Tools.Error

---------------------------------------------------------------------

-- cancel all stuck bookings/rides:
--   bookings, when status is NEW for more than 6 hours
--   bookings and rides, when ride status is NEW for more than 6 hours

postBookingCancelAllStuck ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.StuckBookingsCancelReq ->
  Flow Common.StuckBookingsCancelRes
postBookingCancelAllStuck merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let distanceUnit = merchantOpCity.distanceUnit
  let reqBookingIds = cast @Common.Booking @DBooking.Booking <$> req.bookingIds
  now <- getCurrentTime
  stuckBookings <- B.runInReplica $ QBooking.findStuckBookings merchant merchantOpCity reqBookingIds now
  let stuckBookingIds = (stuckBookings <&> (.id))
  stuckRideItems <- B.runInReplica $ QRide.findStuckRideItems merchant merchantOpCity reqBookingIds now
  bcReasons <- (\booking -> mkBookingCancellationReason merchant.id Common.bookingStuckCode Nothing distanceUnit booking.id booking.riderId) `mapM` stuckBookings
  bcReasonsWithRides <- (\item -> mkBookingCancellationReason merchant.id Common.rideStuckCode (Just item.rideId) distanceUnit item.bookingId item.riderId) `mapM` stuckRideItems
  let allStuckBookingIds = stuckBookingIds <> (stuckRideItems <&> (.bookingId))
  let stuckPersonIds = stuckRideItems <&> (.riderId)
  _ <- QRide.cancelRides (stuckRideItems <&> (.rideId)) now
  _ <- QBooking.cancelBookings allStuckBookingIds now
  _ <- mapM QBPL.makeAllInactiveByBookingId allStuckBookingIds
  for_ (bcReasons <> bcReasonsWithRides) QBCR.upsert
  _ <- QPFS.updateToIdleMultiple stuckPersonIds now
  void $ QPFS.clearCache `mapM` stuckPersonIds
  logTagInfo "dashboard -> stuckBookingsCancel: " $ show allStuckBookingIds
  pure $ mkStuckBookingsCancelRes stuckBookingIds stuckRideItems

mkBookingCancellationReason :: (MonadFlow m) => Id DM.Merchant -> Common.CancellationReasonCode -> Maybe (Id DRide.Ride) -> DistanceUnit -> Id DBooking.Booking -> Id DPerson.Person -> m DBCR.BookingCancellationReason
mkBookingCancellationReason merchantId reasonCode mbRideId distanceUnit bookingId riderId = do
  now <- getCurrentTime
  return $
    DBCR.BookingCancellationReason
      { bookingId = bookingId,
        rideId = mbRideId,
        merchantId = Just merchantId,
        source = DBCR.ByMerchant,
        reasonCode = Just $ coerce @Common.CancellationReasonCode @DCR.CancellationReasonCode reasonCode,
        reasonStage = Nothing,
        additionalInfo = Nothing,
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        distanceUnit,
        riderId = Just riderId,
        createdAt = now,
        updatedAt = now
      }

mkStuckBookingsCancelRes :: [Id DBooking.Booking] -> [QRide.StuckRideItem] -> Common.StuckBookingsCancelRes
mkStuckBookingsCancelRes stuckBookingIds stuckRideItems = do
  let bookingItems = (stuckBookingIds <&>) $ \bookingId -> do
        Common.StuckBookingItem
          { rideId = Nothing,
            bookingId = cast @DBooking.Booking @Common.Booking bookingId
          }
  let rideItems = (stuckRideItems <&>) $ \QRide.StuckRideItem {..} -> do
        Common.StuckBookingItem
          { rideId = Just $ cast @DRide.Ride @Common.Ride rideId,
            bookingId = cast @DBooking.Booking @Common.Booking bookingId
          }
  Common.StuckBookingsCancelRes
    { cancelledBookings = rideItems <> bookingItems
    }

---------------------------------------------------------------------
postBookingSyncMultiple ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MultipleBookingSyncReq ->
  Flow Common.MultipleBookingSyncResp
postBookingSyncMultiple merchantShortId opCity req = do
  runRequestValidation Common.validateMultipleBookingSyncReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  respItems <- forM req.bookings $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      bookingSync merchant merchantOpCity.id reqItem.bookingId
      pure Common.SuccessItem
    pure $ Common.MultipleBookingSyncRespItem {bookingId = reqItem.bookingId, info}
  logTagInfo "dashboard -> multipleBookingSync: " $ show (req.bookings <&> (.bookingId))
  pure $ Common.MultipleBookingSyncResp {list = respItems}

---------------------------------------------------------------------
bookingSync ::
  DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Common.Booking ->
  Flow ()
bookingSync merchant merchantOpCityId reqBookingId = do
  let bookingId = cast @Common.Booking @DBooking.Booking reqBookingId
  booking <- B.runInReplica $ QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (merchant.id == booking.merchantId && merchantOpCityId == booking.merchantOperatingCityId) $
    throwError (BookingDoesNotExist bookingId.getId)

  let merchantOperatingCityId = booking.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= ((.city) <$>) . fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  mbRide <- B.runInReplica $ QRide.findActiveByRBId bookingId
  case mbRide of
    Just ride -> do
      let bookingNewStatus = case ride.status of
            DRide.UPCOMING -> DBooking.TRIP_ASSIGNED
            DRide.NEW -> DBooking.TRIP_ASSIGNED
            DRide.INPROGRESS -> DBooking.TRIP_ASSIGNED
            DRide.COMPLETED -> DBooking.COMPLETED
            DRide.CANCELLED -> DBooking.CANCELLED
      unless (bookingNewStatus == booking.status) $ do
        cancellationReason <- mkBookingCancellationReason merchant.id Common.syncBookingCode (Just ride.id) booking.distanceUnit bookingId booking.riderId
        QBooking.updateStatus bookingId bookingNewStatus
        when (bookingNewStatus == DBooking.CANCELLED) $ QBCR.upsert cancellationReason
      let updBooking = booking{status = bookingNewStatus}
          dStatusReq = DStatusReq {booking = updBooking, merchant, city}
      becknStatusReq <- buildStatusReqV2 dStatusReq
      logTagDebug ("bookingId:-" <> bookingId.getId) $ "bookingSync:-becknStatusReqV2:-" <> show becknStatusReq
      void $ withShortRetry $ CallBPP.callStatusV2 booking.providerUrl becknStatusReq booking.merchantId
    Nothing -> do
      cancellationReason <- mkBookingCancellationReason merchant.id Common.syncBookingCodeWithNoRide Nothing booking.distanceUnit bookingId booking.riderId
      QBooking.updateStatus bookingId DBooking.CANCELLED
      QBCR.upsert cancellationReason
      let updBooking = booking{status = DBooking.CANCELLED}
          dStatusReq = DStatusReq {booking = updBooking, merchant, city}
      becknStatusReq <- buildStatusReqV2 dStatusReq
      logTagDebug ("bookingId:-" <> bookingId.getId) $ "bookingSync:-becknStatusReqv2:-" <> show becknStatusReq
      void $ withShortRetry $ CallBPP.callStatusV2 booking.providerUrl becknStatusReq booking.merchantId
