{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.FRFS.OnCancel (buildOnCancelReq) where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Data.Aeson as A
import qualified Domain.Action.Beckn.FRFS.OnCancel as DOnCancel
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnCancelReq ::
  (MonadFlow m) =>
  Spec.OnCancelReq ->
  m DOnCancel.DOnCancel
buildOnCancelReq onCancelReq = do
  Utils.validateContext Spec.ON_CANCEL onCancelReq.onCancelReqContext
  transactionId <- onCancelReq.onCancelReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- onCancelReq.onCancelReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")

  order <- onCancelReq.onCancelReqMessage <&> (.confirmReqMessageOrder) & fromMaybeM (InvalidRequest "Order not found")
  providerId <- order.orderProvider >>= (.providerId) & fromMaybeM (InvalidRequest "Provider not found")

  item <- order.orderItems >>= listToMaybe & fromMaybeM (InvalidRequest "Item not found")
  bppItemId <- item.itemId & fromMaybeM (InvalidRequest "BppItemId not found")
  bppOrderId <- order.orderId & fromMaybeM (InvalidRequest "BppOrderId not found")

  quotation <- order.orderQuote & fromMaybeM (InvalidRequest "Quotation not found")
  quoteBreakup <- quotation.quotationBreakup & fromMaybeM (InvalidRequest "QuotationBreakup not found")
  totalPrice <- quotation.quotationPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Invalid quotationPrice")

  refundAmount <- getCancellationParams quoteBreakup Spec.REFUND & fromMaybeM (InvalidRequest "CancellationParams Refund Amount not found")
  cancellationCharges <- getCancellationParams quoteBreakup Spec.CANCELLATION_CHARGES & fromMaybeM (InvalidRequest "CancellationParams cancellationCharges not found")
  baseFare <- getCancellationParams quoteBreakup Spec.BASE_FARE & fromMaybeM (InvalidRequest "CancellationParams baseFare not found")

  orderStatus_ <- order.orderStatus & fromMaybeM (InvalidRequest "Order Status not found")
  orderStatus <- (A.decode $ A.encode orderStatus_ :: Maybe Spec.OnCancelOrderStatus) & fromMaybeM (InvalidRequest "Failed to parse orderStatus in onCancel Req")

  pure $
    DOnCancel.DOnCancel
      { providerId,
        totalPrice,
        bppItemId,
        transactionId,
        bppOrderId,
        messageId,
        refundAmount,
        cancellationCharges,
        baseFare,
        orderStatus
      }
  where
    getCancellationParams :: [Spec.QuotationBreakupInner] -> Spec.CancellationParams -> Maybe HighPrecMoney
    getCancellationParams quoteBreakup titleToFind =
      case find (\qb -> qb.quotationBreakupInnerTitle == Just (show titleToFind)) quoteBreakup of
        Just qb -> qb.quotationBreakupInnerPrice >>= Utils.parseMoney
        Nothing -> Nothing
