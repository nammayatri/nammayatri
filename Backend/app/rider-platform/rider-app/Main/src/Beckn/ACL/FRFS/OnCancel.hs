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
import Kernel.Utils.Common
import Tools.Error

buildOnCancelReq ::
  (MonadFlow m) =>
  Spec.OnCancelReq ->
  m (Maybe DOnCancel.DOnCancel)
buildOnCancelReq onCancelReq = do
  Utils.validateContext Spec.ON_CANCEL onCancelReq.onCancelReqContext
  handleError onCancelReq $ \message -> do
    case parseData message of
      Right (providerId, totalPrice, bppItemId, transactionId, bppOrderId, messageId, refundAmount, cancellationCharges, baseFare, orderStatus) -> do
        let dOnCancel =
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
        return $ Just dOnCancel
      Left err -> throwError $ InvalidBecknSchema $ "on_cancel error:-" <> show err
  where
    parseData :: Spec.ConfirmReqMessage -> Either Text (Text, HighPrecMoney, Text, Text, Text, Text, Maybe HighPrecMoney, Maybe HighPrecMoney, HighPrecMoney, Spec.OrderStatus)
    parseData message = do
      transactionId <- onCancelReq.onCancelReqContext.contextTransactionId & maybe (Left "TransactionId not found") Right
      messageId <- onCancelReq.onCancelReqContext.contextMessageId & maybe (Left "MessageId not found") Right

      let order = message.confirmReqMessageOrder
      providerId <- order.orderProvider >>= (.providerId) & maybe (Left "Provider not found") Right

      item <- order.orderItems >>= listToMaybe & maybe (Left "Item not found") Right
      bppItemId <- item.itemId & maybe (Left "BppItemId not found") Right
      bppOrderId <- order.orderId & maybe (Left "BppOrderId not found") Right

      quotation <- order.orderQuote & maybe (Left "Quotation not found") Right
      quoteBreakup <- quotation.quotationBreakup & maybe (Left "QuotationBreakup not found") Right
      totalPrice <- quotation.quotationPrice >>= Utils.parseMoney & maybe (Left "Invalid quotationPrice") Right

      orderStatus_ <- order.orderStatus & maybe (Left "Order Status not found") Right
      orderStatus <- (A.decode $ A.encode orderStatus_ :: Maybe Spec.OrderStatus) & maybe (Left "Failed to parse orderStatus in onCancel Req") Right

      (baseFare, refundAmount, cancellationCharges) <- Utils.getAndValidateCancellationParams quoteBreakup orderStatus

      Right (providerId, totalPrice, bppItemId, transactionId, bppOrderId, messageId, refundAmount, cancellationCharges, baseFare, orderStatus)

handleError ::
  (MonadFlow m) =>
  Spec.OnCancelReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnCancel.DOnCancel)) ->
  m (Maybe DOnCancel.DOnCancel)
handleError req action = do
  case req.onCancelReqError of
    Nothing -> req.onCancelReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_cancel req" $ "on_cancel error:-" <> show err
      pure Nothing
