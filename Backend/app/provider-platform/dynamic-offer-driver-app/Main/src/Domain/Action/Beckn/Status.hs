{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Status
  ( handler,
    DStatusReq (..),
    DStatusRes (..),
  )
where

import Data.Either.Extra (eitherToMaybe)
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as Aadhaar
import Domain.Types.Beckn.Status
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions as B
import Kernel.Tools.Logging
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Beckn.Common as Common
import qualified SharedLogic.SyncRide as SyncRide
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import Tools.Error

handler ::
  Id DM.Merchant ->
  DStatusReq ->
  Flow DStatusRes
handler transporterId req = withDynamicLogLevel "bpp-status-domain" $ do
  logDebug $ "BPP_STATUS_DEBUG: Searching for transactionId: " <> req.transactionId
  transporter <-
    CQM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  booking <- B.runInReplica $ QRB.findByTransactionId req.transactionId >>= fromMaybeM (BookingNotFound req.transactionId)
  logDebug $ "BPP_STATUS_DEBUG: Found booking: " <> booking.id.getId <> " with status: " <> show booking.status
  mbRide <- B.runInReplica $ QRide.findOneByBookingId booking.id
  case mbRide of
    Just ride -> logDebug $ "BPP_STATUS_DEBUG: Found ride: " <> ride.id.getId <> " with status: " <> show ride.status
    Nothing -> logDebug $ "BPP_STATUS_DEBUG: No ride found for booking: " <> booking.id.getId
  let transporterId' = booking.providerId
      estimateId = booking.estimateId <&> getId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  info <- case mbRide of
    Just ride -> do
      case ride.status of
        DRide.UPCOMING -> syncAssignedReq ride booking estimateId
        DRide.NEW -> syncAssignedReq ride booking estimateId
        DRide.INPROGRESS -> do
          bookingDetails <- SyncRide.fetchBookingDetails ride booking
          let tripStartLocation = bookingDetails.ride.tripStartPos
          pure $ RideStartedReq DRideStartedReq {..}
        DRide.COMPLETED -> do
          logDebug $ "BPP_STATUS_DEBUG: Processing COMPLETED ride for booking: " <> booking.id.getId
          bookingDetails <- SyncRide.fetchBookingDetails ride booking
          SyncRide.RideCompletedInfo {..} <- SyncRide.fetchRideCompletedInfo ride booking
          let tripEndLocation = bookingDetails.ride.tripEndPos
          pure $ RideCompletedReq DRideCompletedReq {..}
        DRide.CANCELLED -> do
          case booking.status of
            DBooking.REALLOCATED -> do
              bookingDetails <- SyncRide.fetchBookingDetails ride booking
              bookingReallocationInfo <- SyncRide.fetchBookingReallocationInfo (Just ride) booking
              pure $ BookingReallocationBuildReq DBookingReallocationBuildReq {bookingDetails, bookingReallocationInfo}
            _ -> do
              bookingDetails <- SyncRide.fetchBookingDetails ride booking
              SyncRide.BookingCancelledInfo {..} <- SyncRide.fetchBookingCancelledInfo (Just ride)
              pure $ BookingCancelledReq DBookingCancelledReq {bookingDetails = Just bookingDetails, cancellationFee = Nothing, ..}
    Nothing -> do
      case booking.status of
        DBooking.NEW -> do
          pure $ NewBookingBuildReq (DNewBookingBuildReq booking.id)
        DBooking.TRIP_ASSIGNED -> do
          logDebug $ "BPP_STATUS_DEBUG: ERROR: TRIP_ASSIGNED booking without ride record: " <> booking.id.getId
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
        DBooking.COMPLETED -> do
          logDebug $ "BPP_STATUS_DEBUG: ERROR: COMPLETED booking without ride record: " <> booking.id.getId
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
        DBooking.CANCELLED -> do
          bookingCancelledInfo <- SyncRide.fetchBookingCancelledInfo Nothing
          pure $ BookingCancelledReq DBookingCancelledReq {bookingDetails = Nothing, cancellationFee = Nothing, cancellationSource = bookingCancelledInfo.cancellationSource, ..}
        DBooking.REALLOCATED -> do
          logDebug $ "BPP_STATUS_DEBUG: ERROR: REALLOCATED booking without ride record: " <> booking.id.getId
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
  logDebug $ "BPP_STATUS_DEBUG: Sending response for booking: " <> booking.id.getId
  pure DStatusRes {transporter, booking, info}
  where
    syncAssignedReq ride booking estimateId = do
      bookingDetails <- SyncRide.fetchBookingDetails ride booking
      driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM DriverInfoNotFound
      rideDetails <- runInReplica $ QRideDetails.findById ride.id >>= fromMaybeM (RideNotFound ride.id.getId)
      resp <- withTryCatch "fetchAndCacheAadhaarImage" (Aadhaar.fetchAndCacheAadhaarImage bookingDetails.driver driverInfo)
      let image = join (eitherToMaybe resp)
      let isDriverBirthDay = False
      let isFreeRide = False
      let driverAccountId = Nothing
      let isAlreadyFav = False
      let favCount = 0
      let isSafetyPlus = booking.isSafetyPlus
      pure $ RideAssignedReq DRideAssignedReq {vehicleAge = rideDetails.vehicleAge, ..}
