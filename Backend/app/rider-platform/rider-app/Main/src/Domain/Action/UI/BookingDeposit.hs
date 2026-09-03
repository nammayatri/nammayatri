{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.BookingDeposit
  ( getBookingDepositStatus,
    postBookingDepositPaymentIntent,
    postBookingDepositRefund,
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
import qualified SharedLogic.BookingDeposit as BookingDeposit
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingPayment as QBookingPayment

-- | Shared preamble for both endpoints: authorise the caller, read
--   the fee, and reconcile any in-flight attempt with the gateway.
validateAndSync ::
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
  Kernel.Types.Id.Id DRB.Booking ->
  Environment.Flow (DRB.Booking, HighPrecMoney)
validateAndSync mbPersonId bookingId = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person id is required")
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  fee <- booking.bookingDepositAmount & fromMaybeM (InvalidRequest "Booking has no booking fee")
  mbLatestAttempt <- QBookingPayment.findLatestByBookingIdAndServiceType booking.id DOrder.BookingDeposit
  whenJust mbLatestAttempt $ \row ->
    when (row.status == DBP.PENDING) $
      DPayment.syncBookingDepositOrderStatus booking.merchantId booking.riderId row.paymentOrderId
  pure (booking, fee)

-- | Nudge a booking whose fee is secured but whose Beckn confirm was withheld. Idempotent --
--   the Fulfil lock and the FulfilTriggered marker bound repeat confirms.
resumeIfWithheld :: DRB.Booking -> Environment.Flow ()
resumeIfWithheld booking =
  when (booking.requiresPaymentBeforeConfirm && booking.status == DRB.NEW) $
    fork "bookingDeposit:resumeConfirm" $ DPayment.resumeBookingDepositConfirm booking.id

-- | Poll endpoint. Reconciles, and may SECURE the fee from balance -- that is a PENDING ledger
--   hold, reversible and idempotent, not a gateway order
getBookingDepositStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id DRB.Booking ->
    Environment.Flow API.Types.UI.BookingDeposit.BookingDepositStatusResp
  )
getBookingDepositStatus (mbPersonId, _merchantId) bookingId = do
  (booking, fee) <- validateAndSync mbPersonId bookingId
  mbRow <- QBookingPayment.findLatestByBookingIdAndServiceType booking.id DOrder.BookingDeposit
  captured <- BookingDeposit.depositCaptured booking.id
  if captured || booking.status `elem` DRB.terminalBookingStatus || isRefundState mbRow
    then do
      available <- BookingDeposit.getAvailableBalance booking.riderId
      let st
            | captured = API.Types.UI.BookingDeposit.FORFEITED
            | Just row <- mbRow, row.status == DBP.REFUNDED = API.Types.UI.BookingDeposit.REFUNDED
            | Just row <- mbRow, row.status == DBP.REFUND_FAILED = API.Types.UI.BookingDeposit.REFUND_FAILED
            | isRefundState mbRow = API.Types.UI.BookingDeposit.REFUND_IN_PROGRESS
            | Just row <- mbRow, row.status == DBP.SUCCESS = API.Types.UI.BookingDeposit.REFUND_IN_PROGRESS
            | Just row <- mbRow, row.status == DBP.FAILED = API.Types.UI.BookingDeposit.FAILED
            | otherwise = API.Types.UI.BookingDeposit.REFUNDED
      pure (mkResp fee available st)
    else do
      res <- withTryCatch "getBookingDepositStatus:liveStatus" (liveStatus booking fee)
      case res of
        Right resp -> pure resp
        Left err -> do
          logError $ "Booking fee live status failed for " <> booking.id.getId <> "; returning RETRY: " <> show err
          pure (mkResp fee 0 API.Types.UI.BookingDeposit.RETRY)
  where
    isRefundState = maybe False (\row -> row.status `elem` [DBP.REFUND_PENDING, DBP.REFUND_INITIATED, DBP.REFUND_FAILED])

liveStatus ::
  DRB.Booking ->
  HighPrecMoney ->
  Environment.Flow API.Types.UI.BookingDeposit.BookingDepositStatusResp
liveStatus booking fee = do
  mbDecision <- BookingDeposit.decideAndSecureBookingDeposit booking fee
  case mbDecision of
    Nothing -> pure (mkResp fee 0 API.Types.UI.BookingDeposit.RETRY)
    Just (BookingDeposit.FeeSecured, available) -> do
      resumeIfWithheld booking
      pure (mkResp fee available API.Types.UI.BookingDeposit.COVERED)
    Just (BookingDeposit.FeeShortfall _, available) -> do
      -- Already paid but fulfilment still catching up: say PROCESSING so the client waits
      -- rather than asking for another sheet.
      inFlight <- DPayment.isBookingDepositPaymentInFlight booking
      let status =
            if inFlight
              then API.Types.UI.BookingDeposit.PROCESSING
              else API.Types.UI.BookingDeposit.PAYABLE
      pure (mkResp fee available status)

mkResp :: HighPrecMoney -> HighPrecMoney -> API.Types.UI.BookingDeposit.BookingDepositStatus -> API.Types.UI.BookingDeposit.BookingDepositStatusResp
mkResp fee available status =
  API.Types.UI.BookingDeposit.BookingDepositStatusResp
    { requiredAmount = fee,
      availableBalance = available,
      feeStatus = status
    }

postBookingDepositPaymentIntent ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id DRB.Booking ->
    Environment.Flow API.Types.UI.BookingDeposit.BookingDepositPaymentResp
  )
postBookingDepositPaymentIntent (mbPersonId, _merchantId) bookingId = do
  (booking, fee) <- validateAndSync mbPersonId bookingId
  when (booking.status `elem` DRB.terminalBookingStatus) $
    throwError $ RideInvalidStatus $ "Booking " <> booking.id.getId <> " is " <> show booking.status
  (orderResult, mbAvailableBalance) <- DPayment.createBookingDepositPaymentOrder booking
  let (feeStatus, sdkPayload) = case orderResult of
        DPayment.BookingDepositCoveredByBalance -> (API.Types.UI.BookingDeposit.COVERED, Nothing)
        DPayment.BookingDepositOrderReady resp -> (API.Types.UI.BookingDeposit.PAYABLE, Just resp)
        DPayment.BookingDepositOrderProcessing -> (API.Types.UI.BookingDeposit.PROCESSING, Nothing)
        DPayment.BookingDepositOrderUnavailable -> (API.Types.UI.BookingDeposit.RETRY, Nothing)
  case orderResult of
    DPayment.BookingDepositCoveredByBalance -> resumeIfWithheld booking
    _ -> pure ()
  pure
    API.Types.UI.BookingDeposit.BookingDepositPaymentResp
      { requiredAmount = fee,
        availableBalance = fromMaybe 0 mbAvailableBalance,
        feeStatus = feeStatus,
        sdkPayload = sdkPayload
      }

postBookingDepositRefund ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id DRB.Booking ->
    Environment.Flow API.Types.UI.BookingDeposit.BookingDepositStatusResp
  )
postBookingDepositRefund (mbPersonId, merchantId) bookingId = do
  (booking, _fee) <- validateAndSync mbPersonId bookingId
  unless (booking.status `elem` DRB.terminalBookingStatus) $
    throwError (InvalidRequest $ "Booking is still in status " <> show booking.status <> "; the booking fee can be refunded only after the booking ends")
  captured <- BookingDeposit.depositCaptured booking.id
  when captured $
    throwError (InvalidRequest "Booking fee was forfeited; nothing to refund")
  BookingDeposit.refundBookingDeposit booking
  getBookingDepositStatus (mbPersonId, merchantId) bookingId
