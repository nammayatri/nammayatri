{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.FRFS.OnInit where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnInit as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnInitReq ::
  (MonadFlow m) =>
  Spec.OnInitReq ->
  m Domain.DOnInit
buildOnInitReq onInitReq = do
  -- validate context
  transactionId <- onInitReq.onInitReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- onInitReq.onInitReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")

  order <- onInitReq.onInitReqMessage <&> (.confirmReqMessageOrder) & fromMaybeM (InvalidRequest "Order not found")

  providerId <- order.orderProvider >>= (.providerId) & fromMaybeM (InvalidRequest "Provider not found")

  item <- order.orderItems >>= listToMaybe & fromMaybeM (InvalidRequest "Item not found")
  bppItemId <- item.itemId & fromMaybeM (InvalidRequest "BppItemId not found")
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "BppBookingId not found")

  quotation <- order.orderQuote & fromMaybeM (InvalidRequest "Quotation not found")
  quoteBreakup <- quotation.quotationBreakup & fromMaybeM (InvalidRequest "QuotationBreakup not found")
  totalPrice <- quotation.quotationPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Invalid quotationPrice")

  fareBreakUp <- traverse Utils.mkFareBreakup quoteBreakup

  pure $
    Domain.DOnInit
      { providerId = providerId,
        totalPrice,
        fareBreakUp = fareBreakUp,
        bppItemId,
        transactionId,
        bppBookingId,
        messageId,
        validTill = Nothing -- TODO: fix me
      }
