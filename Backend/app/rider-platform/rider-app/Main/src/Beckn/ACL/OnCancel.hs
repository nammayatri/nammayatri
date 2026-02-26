{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel
  ( buildOnCancelReq,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Data.Maybe (listToMaybe)
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import EulerHS.Prelude hiding (state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnCancelReq ::
  ( HasFlowEnv m r '["_version" ::: Text],
    EsqDBFlow m r
  ) =>
  Spec.OnCancelReq ->
  m (Maybe DOnCancel.OnCancelReq)
buildOnCancelReq req = do
  ContextV2.validateContext Context.ON_CANCEL $ req.onCancelReqContext
  _transactionId <- Utils.getTransactionId req.onCancelReqContext
  handleErrorV2 req $ \message -> do
    bookingCancelledEvent message.confirmReqMessageOrder

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnCancelReq ->
  (Spec.ConfirmReqMessage -> m DOnCancel.OnCancelReq) ->
  m (Maybe DOnCancel.OnCancelReq)
handleErrorV2 req action = do
  onUpdMsg <- req.onCancelReqMessage & fromMaybeM (InvalidRequest "message not present in on_update request.")
  case req.onCancelReqError of
    Nothing -> Just <$> action onUpdMsg
    Just err -> do
      logTagError "on_update req" $ "on_update error: " <> show err
      pure Nothing

bookingCancelledEvent :: (MonadFlow m) => Spec.Order -> m DOnCancel.OnCancelReq
bookingCancelledEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in BookingCancelled Event.")
  let cancellationSource = order.orderCancellation >>= (.cancellationCancelledBy)
  let mbCancellationFee = order.orderCancellationTerms >>= listToMaybe >>= (.cancellationTermCancellationFee) >>= (.feeAmount)
  let cancellationFeeAmount = mbCancellationFee >>= (.priceValue) >>= highPrecMoneyFromText
  let cancellationFeeCurrency :: Maybe Currency = mbCancellationFee >>= (.priceCurrency) >>= readMaybe @Currency
  let cancellationReasonCode = order.orderCancellation >>= (.cancellationReasonCode)
  return $
    DOnCancel.BookingCancelledReq
      { bppBookingId = Id bppBookingId,
        cancellationSource = cancellationSource,
        cancellationFee = cancellationFeeAmount <&> \feeAmount -> PriceAPIEntity feeAmount (fromMaybe INR cancellationFeeCurrency),
        cancellationReasonCode = cancellationReasonCode
      }
