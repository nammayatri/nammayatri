{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.CancellationChargesWaiveOff (postRideBookingCancellationChargesWaiveOff) where

import qualified API.Types.UI.CancellationChargesWaiveOff
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
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
postRideBookingCancellationChargesWaiveOff (mbPersonId, _merchantId) bookingId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  whenJust mbPersonId $ \personId ->
    unless (booking.riderId == personId) $ throwError (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  ride <- QRide.findOneByBookingId bookingId >>= fromMaybeM (RideDoesNotExist bookingId.getId)
  case ride.cancellationChargesOnCancel of
    Just charges | charges > 0 -> do
      case booking.bppBookingId of
        Just bppBookingId -> do
          logInfo $ "CancellationChargesWaiveOff: Trying to waive off cancellation charges for bookingId: " <> bookingId.getId <> " with amount: " <> show charges
          result <- withTryCatch "customerCancellationDuesWaiveOff" $ CallBPPInternal.customerCancellationDuesWaiveOff merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId bppBookingId.getId ride.bppRideId.getId charges
          case result of
            Right _ -> do
              logInfo $ "CancellationChargesWaiveOff: Successfully waived off cancellation charges for bookingId: " <> bookingId.getId
              pure $ mkRes (Just charges) True
            Left err -> do
              logError $ "CancellationChargesWaiveOff: Failed to waive off cancellation charges for bookingId: " <> bookingId.getId <> " with error: " <> show err
              pure $ mkRes Nothing False
        Nothing -> do
          logInfo $ "CancellationChargesWaiveOff: No bppBookingId found for bookingId: " <> bookingId.getId
          pure $ mkRes Nothing False
    _ -> do
      logInfo $ "CancellationChargesWaiveOff: No non-zero cancellation charges found for bookingId: " <> bookingId.getId
      pure $ mkRes Nothing False
  where
    mkRes mbAmt success =
      API.Types.UI.CancellationChargesWaiveOff.CancellationChargesWaiveOffRes
        { waivedOffAmount = mbAmt,
          waivedOffAmountWithCurrency = fmap (\a -> Kernel.Types.Common.PriceAPIEntity {amount = a, currency = Kernel.Types.Common.INR}) mbAmt,
          waivedOffSuccess = success
        }
