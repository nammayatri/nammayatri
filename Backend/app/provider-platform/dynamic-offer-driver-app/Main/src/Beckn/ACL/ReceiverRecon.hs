module Beckn.ACL.ReceiverRecon
  ( buildReceiverReconDomain,
    currencyMismatch,
  )
where

import qualified BecknV2.RSF.Enums as Enums
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

-- Phase 1 (E1): mandatory-field checks while translating the wire payload: a missing field NACKs with 70000 (naming
-- the field), an unparseable amount with 70020. Amounts may be negative (recoveries and deductions), so their sign
-- is not checked. The currency is checked by currencyMismatch once the merchant is known.
-- Phase 1 (E1): an order whose payment.params.currency is given and is not the merchant's currency (70021).
currencyMismatch :: Currency -> Spec.ReceiverReconReq -> Bool
currencyMismatch currency req =
  any
    (\o -> maybe False (/= show currency) (o.rsfOrderPayment >>= (.rsfPaymentParams) >>= (.rsfPaymentParamsCurrency)))
    req.receiverReconReqMessage.rsfOrderbookMessageOrderbook.rsfOrderbookOrders

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
orMissing mVal field = maybe (Left $ RSFUtils.buildNackWithDetail RSFUtils.RSFMissingMandatory (Just field)) Right mVal

invalidAmount :: Text -> Either Spec.RSFAckResponse a
invalidAmount field = Left $ RSFUtils.buildNackWithDetail RSFUtils.RSFInvalidAmount (Just field)

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
    [] -> Left (RSFUtils.buildNackWithDetail RSFUtils.RSFMissingMandatory (Just "settlement_details"))
    (d : ds) -> Right (d :| ds)
  parsedDetails <- traverse buildSettlementDetail nonEmptyDetails
  grossAmountText <- (payment.rsfPaymentParams >>= (.rsfPaymentParamsAmount)) `orMissing` "payment.params.amount"
  claimedGrossAmount <- maybe (invalidAmount "payment.params.amount") Right (RSFUtils.parseMonetaryString grossAmountText)
  pure
    DRecon.ReceiverReconOrder
      { orderId = orderId,
        orderTransactionId = orderTransactionId,
        invoiceNo = order.rsfOrderInvoiceNo,
        collectorAppId = order.rsfOrderCollectorAppId,
        orderState = orderState,
        claimedGrossAmount = claimedGrossAmount,
        paymentStatus = paymentStatus,
        transactionStatus = payment.rsfPaymentParams >>= (.rsfPaymentParamsTransactionStatus),
        settlementId = settlementId,
        reasonCode = Enums.rsfEnumText reasonCode,
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
  let amount = RSFUtils.roundToPaise (toHighPrecMoney rawAmt)
  pure
    DRecon.SettlementDetailParsed
      { utr = ref,
        amount = amount,
        settlementTimestamp = convertRFC3339ToUTC timestamp
      }

extractMonetaryValue :: Maybe Spec.RSFMonetaryValue -> Maybe HighPrecMoney
extractMonetaryValue mbMv = do
  mv <- mbMv
  valText <- mv.rsfMonetaryValueValue
  RSFUtils.parseMonetaryString valText
