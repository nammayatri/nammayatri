module Beckn.ACL.FRFS.OnSelect where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnInit as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildOnSelectReq ::
  (MonadFlow m) =>
  Spec.OnSelectReq ->
  m Domain.DOnInit
buildOnSelectReq onSelectReq = do
  Utils.validateContext Spec.ON_SELECT onSelectReq.onSelectReqContext
  transactionId <- onSelectReq.onSelectReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- onSelectReq.onSelectReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")

  timeStamp <- onSelectReq.onSelectReqContext.contextTimestamp & fromMaybeM (InvalidRequest "Timestamp not found")

  let ttl = onSelectReq.onSelectReqContext.contextTtl >>= Utils.getQuoteValidTill (convertRFC3339ToUTC timeStamp)

  order <- onSelectReq.onSelectReqMessage <&> (.confirmReqMessageOrder) & fromMaybeM (InvalidRequest "Order not found")

  providerId <- order.orderProvider >>= (.providerId) & fromMaybeM (InvalidRequest "Provider not found")

  item <- order.orderItems >>= listToMaybe & fromMaybeM (InvalidRequest "Item not found")
  bppItemId <- item.itemId & fromMaybeM (InvalidRequest "BppItemId not found")

  quotation <- order.orderQuote & fromMaybeM (InvalidRequest "Quotation not found")

  orderPayment <- order.orderPayments >>= listToMaybe & fromMaybeM (InvalidRequest "OrderPayment not found")
  orderPaymentParams <- orderPayment.paymentParams & fromMaybeM (InvalidRequest "PaymentParams not found")
  bankAccNum <- orderPaymentParams.paymentParamsBankAccountNumber & fromMaybeM (InvalidRequest "PaymentParamsBankAccountNumber not found")
  bankCode <- orderPaymentParams.paymentParamsBankCode & fromMaybeM (InvalidRequest "PaymentParamsBankCode not found")

  quoteBreakup <- quotation.quotationBreakup & fromMaybeM (InvalidRequest "QuotationBreakup not found")
  totalPrice <- quotation.quotationPrice >>= Utils.parsePrice & fromMaybeM (InvalidRequest "Invalid quotationPrice")

  fareBreakUp <- traverse Utils.mkFareBreakup quoteBreakup

  pure $
    Domain.DOnInit
      { providerId = providerId,
        totalPrice,
        fareBreakUp = fareBreakUp,
        bppItemId,
        transactionId,
        messageId,
        validTill = ttl,
        bankAccNum,
        bankCode
      }
