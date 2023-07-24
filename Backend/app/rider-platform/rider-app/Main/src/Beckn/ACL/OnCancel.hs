{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel (buildOnCancelReq, handleError) where

import Beckn.ACL.Common
import qualified Beckn.Types.Core.Taxi.OnCancel as OnCancel
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import qualified Domain.Types.BookingCancellationReason as SBCR
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

buildOnCancelReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnCancel.OnCancelMessage ->
  m (Maybe DOnCancel.OnCancelReq)
buildOnCancelReq req = do
  validateContext Context.ON_CANCEL $ req.context
  handleError req.contents $ \message -> do
    _ <- fromMaybeM (InvalidRequest "bookingId not present in booking cancellation request.") message.order.id
    createCancelRequest message.order

createCancelRequest :: (MonadFlow m) => OnCancel.Order -> m DOnCancel.OnCancelReq
createCancelRequest item = do
  tagsGroup <- fromMaybeM (InvalidRequest "cancellation tag is not present in Oncancel Request.") item.fulfillment.tags
  let mbCancellationSource = getTag "cancellation_source_info" "cancellation_source" tagsGroup
  cancellationSource <- fromMaybeM (InvalidRequest "cancellation source does not exist") mbCancellationSource
  return $
    DOnCancel.OnCancelReq
      { bppBookingId = Id (fromMaybe "" item.id), -- here, id will always exist
        cancellationSource = castCancellationSource' cancellationSource
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

castCancellationSource' :: Text -> SBCR.CancellationSource
castCancellationSource' = \case
  "ByUser" -> SBCR.ByUser
  "ByDriver" -> SBCR.ByDriver
  "ByMerchant" -> SBCR.ByMerchant
  "ByAllocator" -> SBCR.ByAllocator
  _ -> SBCR.ByApplication
