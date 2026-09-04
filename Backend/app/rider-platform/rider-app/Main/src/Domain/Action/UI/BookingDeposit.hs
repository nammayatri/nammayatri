{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.BookingDeposit
  ( postBookingDepositPaymentIntent,
  )
where

import qualified API.Types.UI.BookingDeposit
import qualified Domain.Action.UI.Payment as DPayment
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingPayment as DBP
import qualified Domain.Types.BookingStatus as DRB
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (elem, id, whenJust)
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingPayment as QBookingPayment

postBookingDepositPaymentIntent ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BookingDeposit.BookingDepositPaymentIntentReq ->
    Environment.Flow API.Types.UI.BookingDeposit.BookingDepositPaymentIntentResp
  )
postBookingDepositPaymentIntent (mbPersonId, _merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person id is required")
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  when (booking.status `elem` DRB.terminalBookingStatus) $
    throwError $ RideInvalidStatus $ "Booking " <> booking.id.getId <> " is " <> show booking.status
  fee <- booking.bookingDepositAmount & fromMaybeM (InvalidRequest "Booking has no booking fee")
  mbLatestAttempt <- QBookingPayment.findLatestByBookingIdAndServiceType booking.id DOrder.BookingDeposit
  whenJust mbLatestAttempt $ \row ->
    when (row.status == DBP.PENDING) $
      DPayment.syncBookingDepositOrderStatus booking.merchantId booking.riderId row.paymentOrderId
  (orderResult, mbAvailableBalance) <- DPayment.createBookingDepositPaymentOrder booking
  availableBalance <-
    mbAvailableBalance
      & fromMaybeM (InternalError $ "Booking fee balance unavailable for booking " <> booking.id.getId <> "; retry")
  let (feeStatus, sdkPayload) = case orderResult of
        DPayment.BookingDepositCoveredByBalance -> (API.Types.UI.BookingDeposit.COVERED, Nothing)
        DPayment.BookingDepositOrderReady resp -> (API.Types.UI.BookingDeposit.PAYABLE, Just resp)
        DPayment.BookingDepositOrderProcessing -> (API.Types.UI.BookingDeposit.PROCESSING, Nothing)
        DPayment.BookingDepositOrderUnavailable -> (API.Types.UI.BookingDeposit.RETRY, Nothing)
  case orderResult of
    DPayment.BookingDepositCoveredByBalance
      | booking.requiresPaymentBeforeConfirm && booking.status == DRB.NEW ->
        fork "bookingDeposit:resumeConfirm" $ DPayment.resumeBookingDepositConfirm booking.id
    _ -> pure ()
  pure
    API.Types.UI.BookingDeposit.BookingDepositPaymentIntentResp
      { requiredAmount = fee,
        availableBalance = availableBalance,
        feeStatus = feeStatus,
        sdkPayload = sdkPayload
      }
