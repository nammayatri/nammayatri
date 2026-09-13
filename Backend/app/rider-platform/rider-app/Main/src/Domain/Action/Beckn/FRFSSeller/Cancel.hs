module Domain.Action.Beckn.FRFSSeller.Cancel (handleCancel) where

import qualified Beckn.ACL.FRFSSeller.OnCancel as ACL
import qualified Beckn.ACL.FRFSSeller.OnConfirm as OnConfirmACL
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearchACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Monad.Trans.Except (runExceptT, throwE, withExceptT)
import qualified Domain.Action.Beckn.FRFSSeller.Confirm as Confirm
import qualified Domain.Action.Beckn.FRFSSeller.Init as Init
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DTicketStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Environment (Flow)
import qualified ExternalBPP.ExternalAPI.CallAPI as ExternalCallAPI
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import Tools.Error

data CancelPhase = SoftCancel | ConfirmCancel

data CancelFailure
  = CancellationNotPossible Text
  | OrderNotFound Text
  | Unprocessable Text

failureCode :: CancelFailure -> Common.SellerErrorCode
failureCode = \case
  CancellationNotPossible _ -> Common.CancellationNotPossible
  OrderNotFound _ -> Common.OrderNotFound
  Unprocessable _ -> Common.InternalError

failureMessage :: CancelFailure -> Text
failureMessage = \case
  CancellationNotPossible reason -> reason
  OrderNotFound orderId -> "No order found for order id " <> orderId <> ". Send the order id returned in on_confirm."
  Unprocessable reason -> reason

handleCancel :: Text -> Spec.CancelReq -> Flow ()
handleCancel operator req = do
  let ctx = req.cancelReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on cancel context")
  bapUri <- parseBaseUrl bapUriText
  merchant <-
    Common.findSellerMerchant operator
      >>= fromMaybeM (MerchantDoesNotExist operator)
  becknConfig <-
    QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
      >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")
  integratedBPPConfig <- Init.sellerIntegratedBPPConfig merchant.id ctx
  let mbOperatorConfig = integratedBPPConfig.operatorConfig
      self =
        OnSearchACL.SellerIdentity
          { subscriberId = becknConfig.subscriberId,
            subscriberUrl = showBaseUrl becknConfig.subscriberUrl,
            callbackTtl = Common.callbackTtl becknConfig.cancelTTLSec,
            contextVersion = Common.contextVersionOf mbOperatorConfig
          }
  now <- getCurrentTime
  onCancelReq <-
    cancelOrder operator becknConfig mbOperatorConfig integratedBPPConfig req >>= \case
      Right (outcome, order) -> pure $ ACL.buildOnCancelReq self now ctx outcome order
      Left failure -> do
        logWarning $ "FRFS seller cancel rejected: " <> failureMessage failure
        pure $ ACL.buildOnCancelErrorReq self now ctx (Common.becknError (failureCode failure) (failureMessage failure))
  CallBAP.sendOnCancel merchant.id becknConfig.subscriberId bapUri onCancelReq

cancelOrder :: Text -> DBC.BecknConfig -> Maybe Common.OperatorConfig -> DIBC.IntegratedBPPConfig -> Spec.CancelReq -> Flow (Either CancelFailure (ACL.CancelOutcome, OnConfirmACL.ConfirmedOrder))
cancelOrder operator becknConfig mbOperatorConfig integratedBPPConfig req = runExceptT $ do
  let message = req.cancelReqMessage
      orderId = message.cancelReqMessageOrderId
  cfg <- either (throwE . Unprocessable) pure (Common.operatorConfig mbOperatorConfig)
  unless cfg.cancellation.isAllowed $
    throwE (CancellationNotPossible $ "Cannot cancel order id " <> orderId <> ": this operator does not accept cancellations")
  booking <-
    lift (QBooking.findByBppOrderId (Just orderId))
      >>= maybe (throwE (OrderNotFound orderId)) pure
  unless (Common.isSellerRider booking.riderId) $
    throwE (OrderNotFound orderId)
  phase <-
    cancelPhase message
      & maybe (throwE (Unprocessable $ "Unrecognised cancellation type " <> show (message.cancelReqMessageDescriptor >>= (.descriptorCode)))) pure
  when (fromMaybe True cfg.cancellation.technicalOnly && not (isTechnicalCancellation message)) $
    throwE (CancellationNotPossible $ "Cannot cancel ticket for order id " <> orderId <> ": the operator cannot invalidate an issued ticket")
  tickets <- lift (QTicket.findAllByTicketBookingId booking.id)
  let fare = booking.totalPrice.amount
  -- Ask the operator what it will actually refund. Three outcomes, and they must not be conflated. `Right Nothing` is an operator with no
  -- cancellation API (CDAC/CMRL) -- full refund, no charge, same as Go. `Right (Just q)` is a
  -- real quote. `Left err` is a KMRL call that FAILED: refunding the full fare there would
  -- hand back money the operator meant to keep a cancellation charge from, so refuse instead.
  (refund, charges) <-
    if booking.status == DBookingStatus.CANCELLED
      then -- Replay of an already-cancelled order: publish what the ledger stored, never a
      -- fresh quote, or the buyer gets two on_cancels with contradictory figures.
        pure (fromMaybe fare booking.refundAmount, fromMaybe 0 booking.cancellationCharges)
      else
        lift (withTryCatch "frfsSeller:softCancelQuote" (ExternalCallAPI.softCancelTicket integratedBPPConfig (ticketRefIdFor booking tickets))) >>= \case
          Right Nothing -> pure (fare, 0)
          Right (Just quote) ->
            let quoted = realToFrac quote.ticketFare :: HighPrecMoney
             in pure (quoted, max 0 (fare - quoted))
          Left err ->
            throwE (Unprocessable $ "Could not obtain a cancellation quote from the operator for order id " <> orderId <> ": " <> show err)
  case phase of
    SoftCancel -> pure ()
    ConfirmCancel -> unless (booking.status == DBookingStatus.CANCELLED) $ do
      -- Void it upstream where the operator supports it (a no-op for CMRL, which returns
      -- Nothing). A FAILED call must NOT fall through: the rows below mark the booking
      -- CANCELLED and publish a refund, so swallowing the error would refund the fare on a
      -- ticket that is still live at the AFCS gate. Go refuses the same way
      -- (@kochi_metro.go:631-635@ returns the error rather than proceeding).
      lift (withTryCatch "frfsSeller:hardCancel" (ExternalCallAPI.hardCancelTicket integratedBPPConfig (ticketRefIdFor booking tickets))) >>= \case
        Right _ -> pure ()
        Left err -> throwE (Unprocessable $ "Operator refused to cancel order id " <> orderId <> ": " <> show err)
      lift $ QTicket.updateAllStatusByBookingId DTicketStatus.CANCELLED booking.id
      lift $
        QBooking.updateByPrimaryKey
          booking
            { DBooking.status = DBookingStatus.CANCELLED,
              DBooking.customerCancelled = True,
              DBooking.isBookingCancellable = Just False,
              DBooking.refundAmount = Just refund,
              DBooking.cancellationCharges = Just charges
            }
  order <- withExceptT (Unprocessable . Confirm.failureMessage) (Confirm.republish operator becknConfig mbOperatorConfig Nothing booking tickets)
  pure (outcome phase refund charges (length tickets), order)
  where
    -- The operator's own reference for the order: its ticket number where we have one,
    -- otherwise the order id we filed it under.
    ticketRefIdFor booking tickets =
      fromMaybe (fromMaybe "" booking.bppOrderId) (listToMaybe (map (.ticketNumber) tickets))

    outcome phase refund charges ticketCount =
      ACL.CancelOutcome
        { orderStatus = case phase of
            SoftCancel -> SpecEnums.SOFT_CANCELLED
            ConfirmCancel -> SpecEnums.CANCELLED,
          dropTicketTags = case phase of
            SoftCancel -> False
            ConfirmCancel -> True,
          refundAmount = Common.formatPrice (realToFrac refund),
          refundPerTicket = Common.formatPrice (realToFrac refund / fromIntegral (max 1 ticketCount)),
          cancellationCharges = Common.formatPrice (realToFrac charges),
          settlementAmount = Common.formatPrice (realToFrac charges)
        }

isTechnicalCancellation :: Spec.CancelReqMessage -> Bool
isTechnicalCancellation message = message.cancelReqMessageCancellationReasonId == Just "0"

cancelPhase :: Spec.CancelReqMessage -> Maybe CancelPhase
cancelPhase message =
  message.cancelReqMessageDescriptor
    >>= (.descriptorCode)
    >>= \case
      "SOFT_CANCEL" -> Just SoftCancel
      "CONFIRM_CANCEL" -> Just ConfirmCancel
      _ -> Nothing
