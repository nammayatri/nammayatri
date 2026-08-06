module Beckn.ACL.ReceiverRecon
  ( buildReceiverReconDomain,
  )
where

import qualified BecknV2.RSF.Types as Spec
import qualified BecknV2.RSF.Utils as RSFUtils
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Domain.Action.Beckn.ReceiverRecon as DRecon
import Kernel.Prelude
import Kernel.Types.Common ()
import Kernel.Types.TimeRFC339 (convertRFC3339ToUTC)
import Kernel.Utils.Common

-- Phase 1 (E1): mandatory-field checks while translating the wire payload; any gap NACKs with 70000.
buildReceiverReconDomain ::
  A.Value ->
  Spec.ReceiverReconReq ->
  Either Spec.RSFAckResponse DRecon.ReceiverReconRequest
buildReceiverReconDomain rawBody req = do
  let ctx = req.receiverReconReqContext
      wireOrders = req.receiverReconReqMessage.rsfOrderbookMessageOrderbook.rsfOrderbookOrders
  bapId <- ctx.rsfContextBapId `orMissing` "bap_id"
  bapUri <- ctx.rsfContextBapUri `orMissing` "bap_uri"
  messageId <- ctx.rsfContextMessageId `orMissing` "message_id"
  reconTransactionId <- ctx.rsfContextTransactionId `orMissing` "transaction_id"
  timestamp <- ctx.rsfContextTimestamp `orMissing` "timestamp"
  orders <- traverse buildOrder wireOrders
  pure
    DRecon.ReceiverReconRequest
      { bapId = bapId,
        bapUri = bapUri,
        messageId = messageId,
        reconTransactionId = reconTransactionId,
        ttl = ctx.rsfContextTtl,
        contextTimestamp = convertRFC3339ToUTC timestamp,
        rawJson = TL.toStrict . TLE.decodeUtf8 $ A.encode rawBody,
        orders = orders
      }

orMissing :: Maybe a -> Text -> Either Spec.RSFAckResponse a
orMissing mVal _field = maybe (Left $ RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory) Right mVal

buildOrder :: Spec.RSFOrder -> Either Spec.RSFAckResponse DRecon.ReceiverReconOrder
buildOrder order = do
  orderId <- order.rsfOrderId `orMissing` "order id"
  orderTransactionId <- order.rsfOrderTransactionId `orMissing` "order transaction_id"
  orderState <- order.rsfOrderState `orMissing` "order state"
  settlementId <- order.rsfOrderSettlementId `orMissing` "settlement_id"
  _ <- order.rsfOrderSettlementReferenceNo `orMissing` "settlement_reference_no"
  reasonCode <- order.rsfOrderSettlementReasonCode `orMissing` "settlement_reason_code"
  _ <- order.rsfOrderReconStatus `orMissing` "recon_status"
  _ <- order.rsfOrderOrderReconStatus `orMissing` "order_recon_status"
  payment <- order.rsfOrderPayment `orMissing` "payment"
  paymentStatus <- payment.rsfPaymentStatus `orMissing` "payment.status"
  wireDetails <- payment.rsfPaymentSettlementDetails `orMissing` "settlement_details"
  nonEmptyDetails <- case wireDetails of
    [] -> Left (RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory)
    (d : ds) -> Right (d :| ds)
  parsedDetails <- traverse buildSettlementDetail nonEmptyDetails
  let grossAmountText = payment.rsfPaymentParams >>= (.rsfPaymentParamsAmount)
  pure
    DRecon.ReceiverReconOrder
      { orderId = orderId,
        orderTransactionId = orderTransactionId,
        invoiceNo = order.rsfOrderInvoiceNo,
        collectorAppId = order.rsfOrderCollectorAppId,
        orderState = orderState,
        claimedGrossAmount = fromMaybe 0 $ grossAmountText >>= RSFUtils.parseMonetaryString,
        paymentStatus = paymentStatus,
        transactionStatus = payment.rsfPaymentParams >>= (.rsfPaymentParamsTransactionStatus),
        settlementId = settlementId,
        reasonCode = reasonCode,
        bffType = payment.rsfPaymentBuyerAppFinderFeeType,
        bffAmount = payment.rsfPaymentBuyerAppFinderFeeAmount >>= RSFUtils.parseMonetaryString,
        withholdingTaxGst = extractMonetaryValue order.rsfOrderWithholdingTaxGst,
        withholdingTaxTds = extractMonetaryValue order.rsfOrderWithholdingTaxTds,
        deductionByCollector = extractMonetaryValue order.rsfOrderDeductionByCollector,
        settlementDetails = toList parsedDetails
      }

buildSettlementDetail :: Spec.RSFSettlementDetail -> Either Spec.RSFAckResponse DRecon.SettlementDetailParsed
buildSettlementDetail sd = do
  ref <- sd.rsfSettlementDetailReference `orMissing` "settlement_detail.settlement_reference"
  rawAmt <- sd.rsfSettlementDetailAmount `orMissing` "settlement_detail.settlement_amount"
  _ <- sd.rsfSettlementDetailStatus `orMissing` "settlement_detail.settlement_status"
  _ <- sd.rsfSettlementDetailType `orMissing` "settlement_detail.settlement_type"
  timestamp <- sd.rsfSettlementDetailTimestamp `orMissing` "settlement_detail.settlement_timestamp"
  pure
    DRecon.SettlementDetailParsed
      { utr = ref,
        amount = RSFUtils.roundToPaise (toHighPrecMoney rawAmt),
        settlementTimestamp = convertRFC3339ToUTC timestamp
      }

extractMonetaryValue :: Maybe Spec.RSFMonetaryValue -> Maybe HighPrecMoney
extractMonetaryValue mbMv = do
  mv <- mbMv
  valText <- mv.rsfMonetaryValueValue
  RSFUtils.parseMonetaryString valText
