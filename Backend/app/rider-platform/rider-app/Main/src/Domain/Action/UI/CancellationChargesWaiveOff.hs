{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.CancellationChargesWaiveOff (postRideBookingCancellationChargesWaiveOff) where

import qualified API.Types.UI.CancellationChargesWaiveOff
import qualified Domain.Action.Dashboard.Ride as DashboardRide
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error

postRideBookingCancellationChargesWaiveOff ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.Flow API.Types.UI.CancellationChargesWaiveOff.CancellationChargesWaiveOffRes
  )
postRideBookingCancellationChargesWaiveOff (mbPersonId, merchantId) bookingId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.merchantId == merchantId) $ throwError (BookingDoesNotExist bookingId.getId)
  whenJust mbPersonId $ \personId ->
    unless (booking.riderId == personId) $ throwError (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  ride <- QRide.findOneByBookingId bookingId >>= fromMaybeM (RideNotFound $ "No ride found for bookingId: " <> bookingId.getId)
  (mbAmt, success) <- DashboardRide.cancellationChargesWaiveOffCore merchant booking ride
  pure $
    API.Types.UI.CancellationChargesWaiveOff.CancellationChargesWaiveOffRes
      { waivedOffAmount = mbAmt,
        waivedOffAmountWithCurrency = fmap (\a -> Kernel.Types.Common.PriceAPIEntity {amount = a, currency = booking.estimatedFare.currency}) mbAmt,
        waivedOffSuccess = success
      }
