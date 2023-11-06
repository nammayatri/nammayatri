{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Confirm
  ( confirm,
    cancelBooking,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Tools.Notifications as Notify

confirm ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  Id DP.Person ->
  Id DQuote.Quote ->
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  Maybe UTCTime ->
  Maybe Int ->
  m SConfirm.DConfirmRes
confirm personId quoteId paymentMethodId mbStartTime mbRentalDuration = SConfirm.confirm SConfirm.DConfirmReq {..}

-- cancel booking when QUOTE_EXPIRED on bpp side, or other EXTERNAL_API_CALL_ERROR catched
cancelBooking :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r) => DRB.Booking -> m ()
cancelBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  bookingCancellationReason <- buildBookingCancellationReason booking.id
  _ <- QRideB.updateStatus booking.id DRB.CANCELLED
  _ <- QBCR.upsert bookingCancellationReason
  Notify.notifyOnBookingCancelled booking DBCR.ByApplication
  where
    buildBookingCancellationReason bookingId = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = bookingId,
            rideId = Nothing,
            merchantId = Just booking.merchantId,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            reasonStage = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing
          }
