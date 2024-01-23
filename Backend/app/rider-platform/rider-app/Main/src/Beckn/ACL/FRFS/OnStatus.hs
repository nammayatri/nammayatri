{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.FRFS.OnStatus where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.Common as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnStatusReq ::
  (MonadFlow m) =>
  Spec.OnStatusReq ->
  m Domain.DOrder
buildOnStatusReq onStatusReq = do
  -- validate context
  transactionId <- onStatusReq.onStatusReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- onStatusReq.onStatusReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")

  order <- onStatusReq.onStatusReqMessage <&> (.confirmReqMessageOrder) & fromMaybeM (InvalidRequest "Order not found")
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "BppBookingId not found")
  providerId <- order.orderProvider >>= (.providerId) & fromMaybeM (InvalidRequest "Provider not found")

  item <- order.orderItems >>= listToMaybe & fromMaybeM (InvalidRequest "Item not found")
  bppItemId <- item.itemId & fromMaybeM (InvalidRequest "BppItemId not found")

  quotation <- order.orderQuote & fromMaybeM (InvalidRequest "Quotation not found")
  quoteBreakup <- quotation.quotationBreakup & fromMaybeM (InvalidRequest "QuotationBreakup not found")
  totalPrice <- quotation.quotationPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Invalid quotationPrice")
  bppOrderId <- order.orderId & fromMaybeM (InvalidRequest "BppOrderId not found")

  fareBreakUp <- traverse Utils.mkFareBreakup quoteBreakup

  fulfillments <- order.orderFulfillments & fromMaybeM (InvalidRequest "Fulfillments not found")
  when (null fulfillments) $ throwError $ InvalidRequest "Empty fulfillments"
  tickets <- Utils.parseTickets item fulfillments

  pure $
    Domain.DOrder
      { providerId = providerId,
        totalPrice,
        fareBreakUp = fareBreakUp,
        bppItemId,
        bppBookingId,
        transactionId,
        messageId,
        bppOrderId,
        tickets
      }
