{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.FRFS.Update (buildRefundUpdateReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Fields
import Kernel.Utils.Common

buildRefundUpdateReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  Text ->
  DBooking.FRFSTicketBooking ->
  BecknConfig ->
  Utils.BppData ->
  Context.City ->
  Text ->
  HighPrecMoney ->
  Currency ->
  m Spec.UpdateReq
buildRefundUpdateReq messageId booking bapConfig bppData city rrn refundAmount currency = do
  let transactionId = booking.searchId.getId
      ttl = intToNominalDiffTime (fromMaybe 30 bapConfig.cancelTTLSec)
  context <- Utils.buildContext Spec.UPDATE bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData) city booking.vehicleType
  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError $ "bppOrderId not found for booking " <> booking.id.getId)
  pure $
    Spec.UpdateReq
      { updateReqContext = context,
        updateReqMessage =
          Spec.UpdateReqMessage
            { updateReqMessageUpdateTarget = "order.payments",
              updateReqMessageOrder = tfOrder bppOrderId
            }
      }
  where
    tfOrder bppOrderId =
      Spec.Order
        { orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderCreatedAt = Nothing,
          orderFulfillments = Nothing,
          orderId = Just bppOrderId,
          orderItems = Nothing,
          orderPayments = Just [refundPayment],
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderTags = Nothing,
          orderUpdatedAt = Nothing
        }
    refundPayment =
      Spec.Payment
        { paymentCollectedBy = Just "CONSUMER",
          paymentId = Nothing,
          paymentParams =
            Just $
              Spec.PaymentParams
                { paymentParamsAmount = Just $ encodeToText refundAmount,
                  paymentParamsBankAccountNumber = Nothing,
                  paymentParamsBankCode = Nothing,
                  paymentParamsCurrency = Just $ show currency,
                  paymentParamsTransactionId = Just rrn,
                  paymentParamsVirtualPaymentAddress = Nothing
                },
          paymentStatus = Utils.encodeToText' Spec.PAID,
          paymentTags = Nothing,
          paymentType = Just "REFUND"
        }
