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
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Data.Aeson as A
import Control.Lens ((^?), _Just, _head)
import qualified Domain.Action.Beckn.FRFS.Common as Domain
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error

buildOnConfirmReq ::
  (MonadFlow m) =>
  Text ->
  Text ->
  Spec.OnConfirmReq ->
  m (Maybe Domain.DOrder)
buildOnConfirmReq fromStationCode toStationCode onConfirmReq = do
  Utils.validateContext Spec.ON_CONFIRM onConfirmReq.onConfirmReqContext
  handleError onConfirmReq $ \message -> do
    case parseData message of
      Right (providerId, totalPrice, bppItemId, transactionId, bppOrderId, messageId, item, fulfillments, quoteBreakup, orderStatus) -> do
        tickets <- Utils.parseTickets item fulfillments (Just fromStationCode) (Just toStationCode)
        when (null tickets) $ throwError (InvalidBecknSchema $ "No ticket fulfillment found in onconfirm req: " <> show fulfillments)
        fareBreakUp <- traverse Utils.mkFareBreakup quoteBreakup
        let dOrder =
              Domain.DOrder
                { providerId = providerId,
                  totalPrice,
                  fareBreakUp = fareBreakUp,
                  bppItemId,
                  transactionId,
                  orderStatus,
                  bppOrderId,
                  messageId,
                  tickets
                }
        return $ Just dOrder
      Left err -> throwError $ InvalidBecknSchema $ "on_confirm error:-" <> show err
  where
    parseData :: Spec.ConfirmReqMessage -> Either Text (Text, HighPrecMoney, Text, Text, Text, Text, Spec.Item, [Spec.Fulfillment], [Spec.QuotationBreakupInner], Maybe Spec.OrderStatus)
    parseData message = do
      transactionId <- onConfirmReq.onConfirmReqContext.contextTransactionId & maybe (Left "TransactionId not found") Right
      messageId <- onConfirmReq.onConfirmReqContext.contextMessageId & maybe (Left "MessageId not found") Right

      let order = message.confirmReqMessageOrder
      providerId <- order.orderProvider >>= (.providerId) & maybe (Left "Provider not found") Right

      item <- order.orderItems ^? _Just . _head & maybe (Left "Item not found") Right
      bppItemId <- item.itemId & maybe (Left "BppItemId not found") Right
      bppOrderId <- order.orderId & maybe (Left "BppOrderId not found") Right

      quotation <- order.orderQuote & maybe (Left "Quotation not found") Right
      quoteBreakup <- quotation.quotationBreakup & maybe (Left "QuotationBreakup not found") Right
      let orderStatus :: Maybe Spec.OrderStatus = A.decode $ A.encode orderStatus
      totalPrice <- quotation.quotationPrice >>= Utils.parseMoney & maybe (Left "Invalid quotationPrice") Right

      fulfillments <- order.orderFulfillments & maybe (Left "Fulfillments not found") Right
      when (null fulfillments) $ Left "Empty fulfillments"

      Right (providerId, totalPrice, bppItemId, transactionId, bppOrderId, messageId, item, fulfillments, quoteBreakup, orderStatus)

handleError ::
  (MonadFlow m) =>
  Spec.OnConfirmReq ->
  (Spec.ConfirmReqMessage -> m (Maybe Domain.DOrder)) ->
  m (Maybe Domain.DOrder)
handleError req action = do
  case req.onConfirmReqError of
    Nothing -> req.onConfirmReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_confirm req" $ "on_confirm error:-" <> show err
      pure Nothing
