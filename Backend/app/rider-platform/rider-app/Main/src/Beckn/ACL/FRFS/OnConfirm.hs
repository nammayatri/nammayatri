{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.FRFS.OnConfirm (buildOnConfirmReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

data DOrder = DOrder
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    fareBreakUp :: [DFareBreakUp],
    bppItemId :: Text,
    transactionId :: Text,
    messageId :: Text,
    tickets :: [DTicket]
  }

data DFareBreakUp = DFareBreakUp
  { title :: Text,
    price :: HighPrecMoney,
    pricePerUnit :: HighPrecMoney,
    quantity :: Int
  }

data DTicket = DTicket
  { qrData :: Text,
    validTill :: UTCTime,
    status :: Text
  }

buildOnConfirmReq ::
  (MonadFlow m) =>
  Spec.OnConfirmReq ->
  m DOrder
buildOnConfirmReq onConfirmReq = do
  -- validate context
  transactionId <- onConfirmReq.onConfirmReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- onConfirmReq.onConfirmReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")

  order <- onConfirmReq.onConfirmReqMessage <&> (.confirmReqMessageOrder) & fromMaybeM (InvalidRequest "Order not found")

  providerId <- order.orderProvider >>= (.providerId) & fromMaybeM (InvalidRequest "Provider not found")

  item <- order.orderItems >>= listToMaybe & fromMaybeM (InvalidRequest "Item not found")
  bppItemId <- item.itemId & fromMaybeM (InvalidRequest "BppItemId not found")

  quotation <- order.orderQuote & fromMaybeM (InvalidRequest "Quotation not found")
  quoteBreakup <- quotation.quotationBreakup & fromMaybeM (InvalidRequest "QuotationBreakup not found")
  totalPrice <- quotation.quotationPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Invalid quotationPrice")

  fareBreakUp <- traverse mkFareBreakup quoteBreakup

  fulfillments <- order.orderFulfillments & fromMaybeM (InvalidRequest "Fulfillments not found")
  when (null fulfillments) $ throwError $ InvalidRequest "Empty fulfillments"
  tickets <- parseTickets item fulfillments

  pure $
    DOrder
      { providerId = providerId,
        totalPrice,
        fareBreakUp = fareBreakUp,
        bppItemId,
        transactionId,
        messageId,
        tickets
      }

mkFareBreakup :: (MonadFlow m) => Spec.QuotationBreakupInner -> m DFareBreakUp
mkFareBreakup fareBreakup = do
  title <- fareBreakup.quotationBreakupInnerTitle & fromMaybeM (InvalidRequest "Title not found")
  price <- fareBreakup.quotationBreakupInnerPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Price not found")

  breakupItem <- fareBreakup.quotationBreakupInnerItem & fromMaybeM (InvalidRequest "BreakupItem not found")
  let pricePerUnit = breakupItem.itemPrice >>= Utils.parseMoney & fromMaybe price
  let quantity = breakupItem.itemQuantity >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybe 1

  pure $
    DFareBreakUp
      { title,
        price,
        pricePerUnit,
        quantity
      }

parseTickets :: (MonadFlow m) => Spec.Item -> [Spec.Fulfillment] -> m [DTicket]
parseTickets item fulfillments = do
  fulfillmentIds <- item.itemFulfillmentIds & fromMaybeM (InvalidRequest "FulfillmentIds not found")
  when (null fulfillmentIds) $ throwError $ InvalidRequest "Empty fulfillmentIds"

  let ticketFulfillments = filterByIds fulfillmentIds
  when (null ticketFulfillments) $ throwError $ InvalidRequest "No ticket fulfillment found"

  traverse parseTicket ticketFulfillments
  where
    filterByIds fIds = filter (\f -> f.fulfillmentId `elem` (Just <$> fIds)) fulfillments

parseTicket :: (MonadFlow m) => Spec.Fulfillment -> m DTicket
parseTicket fulfillment = do
  stops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "FulfillmentStops not found")
  startStopAuth <- Utils.getStartStop stops >>= (.stopAuthorization) & fromMaybeM (InvalidRequest "StartStop Auth not found")

  qrData <- startStopAuth.authorizationToken & fromMaybeM (InvalidRequest "TicketQrData not found")
  validTill <- startStopAuth.authorizationValidTo & fromMaybeM (InvalidRequest "TicketValidTill not found")
  status <- startStopAuth.authorizationStatus & fromMaybeM (InvalidRequest "TicketStatus not found")

  pure $
    DTicket
      { qrData,
        validTill,
        status
      }
