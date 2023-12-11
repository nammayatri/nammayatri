{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel (buildOnCancelReq) where

import Beckn.ACL.Common (castCancellationSource, getTag)
import qualified Beckn.Types.Core.Taxi.OnCancel as OnCancel
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import EulerHS.Prelude hiding (state)
import Kernel.Product.Validation.Context (validateContext)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnCancelReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnCancel.OnCancelMessage ->
  m (Maybe DOnCancel.OnCancelReq)
buildOnCancelReq req = do
  validateContext Context.ON_CANCEL $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "transaction_id is not present.")
  handleError req.contents $ \message -> do
    tagsGroup <- fromMaybeM (InvalidRequest "tags is not present in OnCancel Req.") message.order.fulfillment.tags
    cancellationReason <-
      fromMaybeM (InvalidRequest "cancellation_reason is not present.") $
        readMaybe . T.unpack
          =<< getTag "cancellation_reasons" "cancellation_reason" tagsGroup
    isOldEstimateValid <-
      fromMaybeM (InvalidRequest "estimate_info is not present.") $
        readMaybe . T.unpack
          =<< getTag "estimate_info" "is_old_estimate_valid" tagsGroup
    return $
      DOnCancel.OnCancelReq
        { searchRequestId = Id transactionId,
          bppEstimateId = Id message.order.item.id,
          bppBookingId = Id $ message.order.id,
          bppRideId = Id message.order.fulfillment.id,
          cancellationSource = castCancellationSource cancellationReason,
          ..
        }

handleError ::
  (MonadFlow m) =>
  Either Error OnCancel.OnCancelMessage ->
  (OnCancel.OnCancelMessage -> m DOnCancel.OnCancelReq) ->
  m (Maybe DOnCancel.OnCancelReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_cancel req" $ "on_cancel error: " <> show err
      pure Nothing
