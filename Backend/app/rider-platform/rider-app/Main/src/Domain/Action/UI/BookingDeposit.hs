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
import EulerHS.Prelude hiding (elem, id, null, whenJust)
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.BookingDeposit as BookingDeposit
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingPayment as QBookingPayment
import Tools.Error (BookingDepositError (..))

-- | Shared preamble for both endpoints: authorise the caller, read the fee, and reconcile any in-flight attempt with the gateway.
--   Returns the latest attempt row as it stands after that sync, and whether that sync reached the gateway.
validateAndSync ::
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
  Kernel.Types.Id.Id DRB.Booking ->
  Environment.Flow (DRB.Booking, HighPrecMoney, Maybe DBP.BookingPayment, Bool)
validateAndSync mbPersonId bookingId = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person id is required")
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  fee <- booking.bookingDepositAmount & fromMaybeM (InvalidRequest "Booking has no booking fee")
  mbLatestAttempt <- QBookingPayment.findLatestByBookingIdAndServiceType booking.id DOrder.BookingDeposit
  case mbLatestAttempt of
    Just row | row.status == DBP.PENDING -> do
      synced <- DPayment.syncBookingDepositOrderStatus booking.merchantId booking.riderId row.paymentOrderId
      mbRow <- QBookingPayment.findLatestByBookingIdAndServiceType booking.id DOrder.BookingDeposit
      pure (booking, fee, mbRow, synced)
    _ -> pure (booking, fee, mbLatestAttempt, True)

resumeIfWithheld :: DRB.Booking -> Environment.Flow ()
resumeIfWithheld booking =
  when (booking.requiresPaymentBeforeConfirm && booking.status == DRB.NEW) $ do
    triggered <- BookingDeposit.isBookingDepositConfirmTriggered booking.id
    unless triggered $
      fork "bookingDeposit:resumeConfirm" $ DPayment.resumeBookingDepositConfirm booking.id

-- | Poll endpoint. Reconciles an in-flight payment with the gateway; otherwise read-only -- fee from wallet balance is secured only on explicit actions (confirm, paymentIntent)
getBookingDepositStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id DRB.Booking ->
    Environment.Flow API.Types.UI.BookingDeposit.BookingDepositStatusResp
  )
getBookingDepositStatus (mbPersonId, _merchantId) bookingId = do
  (booking, fee, mbRow, synced) <- validateAndSync mbPersonId bookingId
  resp <- statusFromRows booking fee mbRow
  when (not synced && resp.feeStatus `elem` [API.Types.UI.BookingDeposit.PAYABLE, API.Types.UI.BookingDeposit.FAILED]) $
    throwError $ BookingDepositGatewaySyncFailed booking.id.getId
  pure resp

statusFromRows ::
  DRB.Booking ->
  HighPrecMoney ->
  Maybe DBP.BookingPayment ->
  Environment.Flow API.Types.UI.BookingDeposit.BookingDepositStatusResp
statusFromRows booking fee mbRow = do
  (captured, holds) <- BookingDeposit.depositHoldState booking.id
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
      res <- withTryCatch "getBookingDepositStatus:liveStatus" (liveStatus booking fee holds mbRow)
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
  [LE.LedgerEntry] ->
  Maybe DBP.BookingPayment ->
  Environment.Flow API.Types.UI.BookingDeposit.BookingDepositStatusResp
liveStatus booking fee holds mbRow = do
  available <- BookingDeposit.getAvailableBalance booking.riderId
  case holds of
    (_ : _) -> do
      resumeIfWithheld booking
      pure (mkResp fee available API.Types.UI.BookingDeposit.COVERED)
    [] -> do
      (inFlight, attemptFailed) <- BookingDeposit.depositAttemptVerdict mbRow
      let status
            | inFlight = API.Types.UI.BookingDeposit.PROCESSING
            | attemptFailed = API.Types.UI.BookingDeposit.FAILED
            | otherwise = API.Types.UI.BookingDeposit.PAYABLE
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
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow API.Types.UI.BookingDeposit.BookingDepositPaymentResp
  )
postBookingDepositPaymentIntent (mbPersonId, _merchantId) bookingId mbIsMockPayment = do
  (booking, fee, _, synced) <- validateAndSync mbPersonId bookingId
  when (booking.status `elem` DRB.terminalBookingStatus) $
    throwError $ RideInvalidStatus $ "Booking " <> booking.id.getId <> " is " <> show booking.status
  (orderResult, mbAvailableBalance) <-
    if synced
      then DPayment.createBookingDepositPaymentOrder booking (fromMaybe False mbIsMockPayment)
      else do
        -- The pending attempt was not checked with the gateway and may already be paid there: answer RETRY, never its sheet.
        logError $ "Booking fee payment intent for " <> booking.id.getId <> ": pending attempt not verified with the gateway; returning RETRY"
        available <- BookingDeposit.getAvailableBalance booking.riderId
        pure (DPayment.BookingDepositOrderUnavailable, Just available)
  (feeStatus, sdkPayload) <- case orderResult of
    DPayment.BookingDepositCoveredByBalance -> pure (API.Types.UI.BookingDeposit.COVERED, Nothing)
    DPayment.BookingDepositOrderReady resp -> pure (API.Types.UI.BookingDeposit.PAYABLE, Just resp)
    DPayment.BookingDepositOrderProcessing -> pure (API.Types.UI.BookingDeposit.PROCESSING, Nothing)
    DPayment.BookingDepositOrderUnavailable -> pure (API.Types.UI.BookingDeposit.RETRY, Nothing)
    DPayment.BookingDepositPayWindowClosed -> throwError $ InvalidRequest "The payment window for this booking has closed"
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
  (booking, _fee, _, _) <- validateAndSync mbPersonId bookingId
  unless (booking.status `elem` DRB.terminalBookingStatus) $
    throwError (InvalidRequest $ "Booking is still in status " <> show booking.status <> "; the booking fee can be refunded only after the booking ends")
  captured <- fst <$> BookingDeposit.depositHoldState booking.id
  when captured $
    throwError (InvalidRequest "Booking fee was forfeited; nothing to refund")
  BookingDeposit.refundBookingDeposit booking
  getBookingDepositStatus (mbPersonId, merchantId) bookingId
