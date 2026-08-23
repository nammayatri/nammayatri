module Domain.Action.Beckn.FRFSSeller.Status (handleStatus) where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import qualified Beckn.ACL.FRFSSeller.OnConfirm as OnConfirmACL
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearchACL
import qualified Beckn.ACL.FRFSSeller.OnStatus as ACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Monad.Trans.Except (runExceptT, throwE, withExceptT)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFSSeller.Confirm as Confirm
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FRFSTicket as DTicket
import qualified Domain.Types.FRFSTicketStatus as DTicketStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Environment (Flow)
import qualified ExternalBPP.ExternalAPI.CallAPI as ExternalCallAPI
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

data StatusFailure
  = OrderNotFound Text
  | Unprocessable Text

failureCode :: StatusFailure -> Common.SellerErrorCode
failureCode = \case
  OrderNotFound _ -> Common.OrderNotFound
  Unprocessable _ -> Common.InternalError

failureMessage :: StatusFailure -> Text
failureMessage = \case
  OrderNotFound orderId -> "No order found for order id " <> orderId <> ". Send the order id returned in on_confirm."
  Unprocessable reason -> reason

handleStatus :: Text -> Spec.StatusReq -> Flow ()
handleStatus operator req = do
  let ctx = req.statusReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on status context")
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
  now <- getCurrentTime
  onStatusReq <-
    reportOrder operator becknConfig req >>= \case
      Right (orderStatus, order) -> pure $ ACL.buildOnStatusReq self now ctx orderStatus order
      Left failure -> do
        logWarning $ "FRFS seller status rejected: " <> failureMessage failure
        pure $ ACL.buildOnStatusErrorReq self now ctx (Common.becknError (failureCode failure) (failureMessage failure))
  CallBAP.sendOnStatus merchant.id becknConfig.subscriberId bapUri onStatusReq

reportOrder :: Text -> DBC.BecknConfig -> Spec.StatusReq -> Flow (Either StatusFailure (SpecEnums.OrderStatus, OnConfirmACL.ConfirmedOrder))
reportOrder operator becknConfig req = runExceptT $ do
  let orderId = req.statusReqMessage.statusReqMessageOrderId
  booking <-
    lift (QBooking.findByBppOrderId (Just orderId))
      >>= maybe (throwE (OrderNotFound orderId)) pure
  unless (Common.isSellerRider booking.riderId) $
    throwE (OrderNotFound orderId)
  tickets <- lift (QTicket.findAllByTicketBookingId booking.id)
  integratedBPPConfig <-
    lift (SIBC.findByIdCP booking.integratedBppConfigId)
      >>= maybe (throwE (Unprocessable $ "No integrated BPP config " <> booking.integratedBppConfigId.getId)) pure
  refreshed <- lift (mapM (refreshTicket integratedBPPConfig) tickets)
  order <-
    withExceptT
      statusFailure
      ( Confirm.republish operator becknConfig integratedBPPConfig.operatorConfig Nothing booking refreshed
      )
  pure (deriveOrderStatus refreshed, order)
  where
    statusFailure = Unprocessable . Confirm.failureMessage

refreshTicket :: DIBC.IntegratedBPPConfig -> DTicket.FRFSTicket -> Flow DTicket.FRFSTicket
refreshTicket integratedBPPConfig ticket
  | isTerminal ticket.status = pure ticket
  | otherwise = do
    result <- withTryCatch "frfsSeller:refreshTicket" (ExternalCallAPI.getTicketDetailStatusCode integratedBPPConfig ticket.ticketNumber)
    case result of
      Right (Just code) -> do
        let refreshed = operatorCodeToStatus code ticket.status
        -- Persist it: otherwise the operator's verdict lives only in this response, every later
        -- status request re-asks the operator, and no other consumer ever sees the new state.
        when (refreshed /= ticket.status) $
          QTicket.updateStatusByTBookingIdAndTicketNumber refreshed ticket.scannedByVehicleNumber ticket.frfsTicketBookingId ticket.ticketNumber
        pure ticket{DTicket.status = refreshed}
      Right Nothing -> pure ticket
      Left err -> do
        Metrics.incrementExternalProviderFailure integratedBPPConfig.agencyKey "ticketStatus" "exception"
        logWarning $ "FRFS seller status: ticket " <> ticket.ticketNumber <> " not refreshed, keeping stored status: " <> show err
        pure ticket
  where
    isTerminal st = st `elem` [DTicketStatus.CANCELLED, DTicketStatus.COUNTER_CANCELLED]

-- | Accepts the RAW operator vocabularies that 'ExternalCallAPI.getTicketDetailStatusCode' can
-- return: CMRLV2's NEW/ENTRY_USED/USED/EXPIRED/CANCELLED and KMRL's UNUSED/USED/cancelled/EXPIRED
-- (the kochi status map at
-- public-transport-bpp/internal/app/publicTransport/metro/kochi_metro.go:475). Go keys that map
-- on a lowercase "cancelled"; upcasing first is deliberately more lenient and covers both.
operatorCodeToStatus :: Text -> DTicketStatus.FRFSTicketStatus -> DTicketStatus.FRFSTicketStatus
operatorCodeToStatus code fallback = case T.toUpper code of
  "NEW" -> DTicketStatus.ACTIVE
  "UNUSED" -> DTicketStatus.ACTIVE
  "ENTRY_USED" -> DTicketStatus.INPROGRESS
  "USED" -> DTicketStatus.USED
  "EXPIRED" -> DTicketStatus.EXPIRED
  "CANCELLED" -> DTicketStatus.CANCELLED
  _ -> fallback

deriveOrderStatus :: [DTicket.FRFSTicket] -> SpecEnums.OrderStatus
deriveOrderStatus tickets
  | null wireStatuses = SpecEnums.CANCELLED
  | all (== "CANCELLED") wireStatuses = SpecEnums.CANCELLED
  | "UNCLAIMED" `elem` wireStatuses = SpecEnums.ACTIVE
  | otherwise = SpecEnums.COMPLETE
  where
    wireStatuses = map (ACLUtils.wireTicketStatus . (.status)) tickets
