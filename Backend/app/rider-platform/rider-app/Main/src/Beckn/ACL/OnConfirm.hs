{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmReq, buildOnConfirmReqV2) where

import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

buildOnConfirmReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    MonadFlow m
  ) =>
  OnConfirm.OnConfirmReq ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmReq req = do
  validateContext Context.ON_CONFIRM req.context
  handleError req.contents $ \message -> do
    return $
      DOnConfirm.OnConfirmReq
        { bppBookingId = Id message.order.id,
          specialZoneOtp = case message.order.fulfillment.start.authorization of
            Nothing -> Nothing
            Just auth -> Just $ auth.token
        }

buildOnConfirmReqV2 ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnConfirm.OnConfirmReqV2 ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmReqV2 req = do
  validateContext Context.ON_CONFIRM req.context
  handleErrorV2 req.contents $ \message -> do
    fulfillment <- case message.order.fulfillments of
      [fulfillment] -> pure fulfillment
      _ -> throwError . InvalidRequest $ "Exactly one fulfillment is expected " <> show message.order.fulfillments
    start <- find (\stop -> stop.stopType == OnConfirm.START) fulfillment.stops & fromMaybeM (InvalidRequest $ "start stop missing " <> show fulfillment.stops)
    return $
      DOnConfirm.OnConfirmReq
        { bppBookingId = Id message.order.id,
          specialZoneOtp = case start.authorization of
            Nothing -> Nothing
            Just auth -> Just $ auth.token
        }

handleError ::
  (MonadFlow m) =>
  Either Error OnConfirm.OnConfirmMessage ->
  (OnConfirm.OnConfirmMessage -> m DOnConfirm.OnConfirmReq) ->
  m (Maybe DOnConfirm.OnConfirmReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_confirm req" $ "on_confirm error: " <> show err
      pure Nothing

handleErrorV2 ::
  (MonadFlow m) =>
  Either Error OnConfirm.OnConfirmMessageV2 ->
  (OnConfirm.OnConfirmMessageV2 -> m DOnConfirm.OnConfirmReq) ->
  m (Maybe DOnConfirm.OnConfirmReq)
handleErrorV2 etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_confirm req" $ "on_confirm error: " <> show err
      pure Nothing
