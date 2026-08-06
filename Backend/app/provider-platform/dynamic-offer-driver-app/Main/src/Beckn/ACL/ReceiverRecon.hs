module Beckn.ACL.ReceiverRecon
  ( buildReceiverReconDomain,
  )
where

import qualified BecknV2.RSF.Types as Spec
import qualified BecknV2.RSF.Utils as RSFUtils
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock (UTCTime (..))
import qualified Domain.Action.Beckn.ReceiverRecon as DRecon
import Kernel.Prelude
import Kernel.Types.Common ()
import Kernel.Types.Error
import Kernel.Types.TimeRFC339 (convertRFC3339ToUTC)
import Kernel.Utils.Common

buildReceiverReconDomain ::
  (MonadFlow m) =>
  Spec.ReceiverReconReq ->
  m DRecon.ReceiverReconRequest
buildReceiverReconDomain req = do
  now <- getCurrentTime
  let ctx = req.receiverReconReqContext
  bapId <- ctx.rsfContextBapId & fromMaybeM (InvalidRequest "Missing bap_id")
  bapUri <- ctx.rsfContextBapUri & fromMaybeM (InvalidRequest "Missing bap_uri")
  messageId <- ctx.rsfContextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  reconTransactionId <- ctx.rsfContextTransactionId & fromMaybeM (InvalidRequest "Missing transaction_id")
  let deadline = fromMaybe (addUTCTime 86400 now) $ RSFUtils.parseDeadline now ctx.rsfContextTtl
  let wireOrders = req.receiverReconReqMessage.rsfOrderbookMessageOrderbook.rsfOrderbookOrders
  orders <- mapM buildOrder wireOrders
  pure
    DRecon.ReceiverReconRequest
      { bapId = bapId,
        bapUri = bapUri,
        messageId = messageId,
        reconTransactionId = reconTransactionId,
        deadline = deadline,
        orders = orders
      }

buildOrder :: (MonadFlow m) => Spec.RSFOrder -> m DRecon.ReceiverReconOrder
buildOrder order = do
  orderId <- order.rsfOrderId & fromMaybeM (InvalidRequest "Missing order id")
  let payment = order.rsfOrderPayment
      grossAmountText = payment >>= (.rsfPaymentParams) >>= (.rsfPaymentParamsAmount)
      claimedGrossAmount = fromMaybe 0 $ grossAmountText >>= RSFUtils.parseMonetaryString
      settlementDetails = fromMaybe [] $ payment >>= (.rsfPaymentSettlementDetails)
      parsedDetails = mapMaybe buildSettlementDetail settlementDetails
      claimedSettlementAmount = sum $ map (.amount) parsedDetails
      paymentStatus = fromMaybe "NOT-PAID" $ payment >>= (.rsfPaymentStatus)
      bffType = payment >>= (.rsfPaymentBuyerAppFinderFeeType)
      rawBffValue = payment >>= (.rsfPaymentBuyerAppFinderFeeAmount) >>= RSFUtils.parseMonetaryString
      bffAmount = case bffType of
        Just "Percentage" -> do
          pct <- rawBffValue
          pure $ claimedGrossAmount * pct / 100
        _ -> rawBffValue
      rawJson = TL.toStrict . TLE.decodeUtf8 $ A.encode order
  pure
    DRecon.ReceiverReconOrder
      { orderId = orderId,
        orderTransactionId = fromMaybe "" $ order.rsfOrderTransactionId,
        invoiceNo = order.rsfOrderInvoiceNo,
        orderState = fromMaybe "Created" $ order.rsfOrderState,
        claimedGrossAmount = claimedGrossAmount,
        claimedSettlementAmount = claimedSettlementAmount,
        paymentStatus = paymentStatus,
        settlementId = fromMaybe "" $ order.rsfOrderSettlementId,
        settlementType = fromMaybe "neft" $ headMay parsedDetails >>= Just . (.sdSettlementType),
        settlementDate = fromMaybe (UTCTime (toEnum 0) 0) $ headMay parsedDetails >>= Just . (.sdSettlementDate),
        settlementReferenceNo = fromMaybe "" $ order.rsfOrderSettlementReferenceNo,
        reasonCode = fromMaybe "" $ order.rsfOrderSettlementReasonCode,
        wireReconStatus = fromMaybe "" $ order.rsfOrderReconStatus,
        wireOrderReconStatus = fromMaybe "" $ order.rsfOrderOrderReconStatus,
        bffType = bffType,
        bffAmount = bffAmount,
        withholdingTaxGst = extractMonetaryValue order.rsfOrderWithholdingTaxGst,
        withholdingTaxTds = extractMonetaryValue order.rsfOrderWithholdingTaxTds,
        deductionByCollector = extractMonetaryValue order.rsfOrderDeductionByCollector,
        rawJson = rawJson,
        settlementDetails = parsedDetails
      }

buildSettlementDetail :: Spec.RSFSettlementDetail -> Maybe DRecon.SettlementDetailParsed
buildSettlementDetail sd = do
  ref <- sd.rsfSettlementDetailReference
  rawAmt <- sd.rsfSettlementDetailAmount
  let amt = realToFrac rawAmt :: HighPrecMoney
  pure
    DRecon.SettlementDetailParsed
      { utr = ref,
        amount = amt,
        status = fromMaybe "" sd.rsfSettlementDetailStatus,
        sdSettlementType = fromMaybe "neft" sd.rsfSettlementDetailType,
        sdSettlementDate = maybe (UTCTime (toEnum 0) 0) convertRFC3339ToUTC sd.rsfSettlementDetailTimestamp
      }

extractMonetaryValue :: Maybe Spec.RSFMonetaryValue -> Maybe HighPrecMoney
extractMonetaryValue mbMv = do
  mv <- mbMv
  valText <- mv.rsfMonetaryValueValue
  RSFUtils.parseMonetaryString valText
