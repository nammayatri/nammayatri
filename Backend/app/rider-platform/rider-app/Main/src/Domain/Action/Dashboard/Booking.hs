{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Booking
  ( stuckBookingsCancel,
    multipleBookingSync,
  )
where

import Beckn.ACL.Status
import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import Data.Coerce (coerce)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantConfigNew as DMC
import qualified Domain.Types.Ride as DRide
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.CachedQueries.Merchant.MerchantConfigNew as CQMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QRide

---------------------------------------------------------------------

-- cancel all stuck bookings/rides:
--   bookings, when status is NEW for more than 6 hours
--   bookings and rides, when ride status is NEW for more than 6 hours

stuckBookingsCancel ::
  ShortId DM.Merchant ->
  Common.StuckBookingsCancelReq ->
  Flow Common.StuckBookingsCancelRes
stuckBookingsCancel merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let reqBookingIds = cast @Common.Booking @DBooking.Booking <$> req.bookingIds
  now <- getCurrentTime
  stuckBookingIds <- B.runInReplica $ QBooking.findStuckBookings merchant.id reqBookingIds now
  stuckRideItems <- B.runInReplica $ QRide.findStuckRideItems merchant.id reqBookingIds now
  let bcReasons = mkBookingCancellationReason merchant.id Common.bookingStuckCode Nothing <$> stuckBookingIds
  let bcReasonsWithRides = (\item -> mkBookingCancellationReason (merchant.id) Common.rideStuckCode (Just item.rideId) item.bookingId) <$> stuckRideItems
  let allStuckBookingIds = stuckBookingIds <> (stuckRideItems <&> (.bookingId))
  let stuckPersonIds = stuckRideItems <&> (.riderId)
  _ <- QRide.cancelRides (stuckRideItems <&> (.rideId)) now
  _ <- QBooking.cancelBookings allStuckBookingIds now
  for_ (bcReasons <> bcReasonsWithRides) QBCR.upsert
  _ <- QPFS.updateToIdleMultiple stuckPersonIds now
  void $ QPFS.clearCache `mapM` stuckPersonIds
  logTagInfo "dashboard -> stuckBookingsCancel: " $ show allStuckBookingIds
  pure $ mkStuckBookingsCancelRes stuckBookingIds stuckRideItems

mkBookingCancellationReason :: Id DM.Merchant -> Common.CancellationReasonCode -> Maybe (Id DRide.Ride) -> Id DBooking.Booking -> DBCR.BookingCancellationReason
mkBookingCancellationReason merchantId reasonCode mbRideId bookingId = do
  DBCR.BookingCancellationReason
    { bookingId = bookingId,
      rideId = mbRideId,
      merchantId = Just merchantId,
      source = DBCR.ByMerchant,
      reasonCode = Just $ coerce @Common.CancellationReasonCode @DCR.CancellationReasonCode reasonCode,
      reasonStage = Nothing,
      additionalInfo = Nothing,
      driverCancellationLocation = Nothing,
      driverDistToPickup = Nothing
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
multipleBookingSync ::
  ShortId DM.Merchant ->
  Common.MultipleBookingSyncReq ->
  Flow Common.MultipleBookingSyncResp
multipleBookingSync merchantShortId req = do
  runRequestValidation Common.validateMultipleBookingSyncReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantConfig <- CQMC.findByMerchantId merchant.id >>= fromMaybeM (MerchantDoesNotExist merchant.id.getId)
  respItems <- forM req.bookings $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      bookingSync merchant merchantConfig reqItem.bookingId
      pure Common.SuccessItem
    pure $ Common.MultipleBookingSyncRespItem {bookingId = reqItem.bookingId, info}
  logTagInfo "dashboard -> multipleBookingSync: " $ show (req.bookings <&> (.bookingId))
  pure $ Common.MultipleBookingSyncResp {list = respItems}

---------------------------------------------------------------------
bookingSync ::
  DM.Merchant ->
  DMC.MerchantConfigNew ->
  Id Common.Booking ->
  Flow ()
bookingSync merchant merchantConfig reqBookingId = do
  let bookingId = cast @Common.Booking @DBooking.Booking reqBookingId
  booking <- B.runInReplica $ QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (merchant.id == booking.merchantId) $
    throwError (BookingDoesNotExist bookingId.getId)

  mbRide <- B.runInReplica $ QRide.findActiveByRBId bookingId
  case mbRide of
    Just ride -> do
      let bookingNewStatus = case ride.status of
            DRide.NEW -> DBooking.TRIP_ASSIGNED
            DRide.INPROGRESS -> DBooking.TRIP_ASSIGNED
            DRide.COMPLETED -> DBooking.COMPLETED
            DRide.CANCELLED -> DBooking.CANCELLED
      unless (bookingNewStatus == booking.status) $ do
        let cancellationReason = mkBookingCancellationReason merchant.id Common.syncBookingCode (Just ride.id) bookingId
        QBooking.updateStatus bookingId bookingNewStatus
        when (bookingNewStatus == DBooking.CANCELLED) $ QBCR.upsert cancellationReason
      let updBooking = booking{status = bookingNewStatus}
      let dStatusReq = DStatusReq {booking = updBooking, merchant, merchantConfig}
      becknStatusReq <- buildStatusReq dStatusReq
      void $ withShortRetry $ CallBPP.callStatus booking.providerUrl becknStatusReq
    Nothing -> do
      let cancellationReason = mkBookingCancellationReason merchant.id Common.syncBookingCodeWithNoRide Nothing bookingId
      QBooking.updateStatus bookingId DBooking.CANCELLED
      QBCR.upsert cancellationReason
