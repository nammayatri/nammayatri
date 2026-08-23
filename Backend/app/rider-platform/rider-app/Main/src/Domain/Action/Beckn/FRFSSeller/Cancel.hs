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
import Environment (Flow)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
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
    CQM.findByShortId (Common.operatorMerchantShortId operator)
      >>= fromMaybeM (MerchantDoesNotExist operator)
  becknConfig <-
    QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
      >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")
  let self =
        OnSearchACL.SellerIdentity
          { subscriberId = becknConfig.subscriberId,
            subscriberUrl = showBaseUrl becknConfig.subscriberUrl
          }
  mbOperatorConfig <- (.operatorConfig) <$> Init.sellerIntegratedBPPConfig merchant.id ctx
  now <- getCurrentTime
  onCancelReq <-
    cancelOrder operator becknConfig mbOperatorConfig req >>= \case
      Right (outcome, order) -> pure $ ACL.buildOnCancelReq self now ctx outcome order
      Left failure -> do
        logWarning $ "FRFS seller cancel rejected: " <> failureMessage failure
        pure $ ACL.buildOnCancelErrorReq self now ctx (Common.becknError (failureCode failure) (failureMessage failure))
  CallBAP.sendOnCancel merchant.id becknConfig.subscriberId bapUri onCancelReq

cancelOrder :: Text -> DBC.BecknConfig -> Maybe Common.OperatorConfig -> Spec.CancelReq -> Flow (Either CancelFailure (ACL.CancelOutcome, OnConfirmACL.ConfirmedOrder))
cancelOrder operator becknConfig mbOperatorConfig req = runExceptT $ do
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
  unless (isTechnicalCancellation message) $
    throwE (CancellationNotPossible $ "Cannot cancel ticket for order id " <> orderId <> ": the operator cannot invalidate an issued ticket")
  tickets <- lift (QTicket.findAllByTicketBookingId booking.id)
  let fare = booking.totalPrice.amount
  let refund = fare
      charges = 0 :: HighPrecMoney
  case phase of
    SoftCancel -> pure ()
    ConfirmCancel -> unless (booking.status == DBookingStatus.CANCELLED) $ do
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
