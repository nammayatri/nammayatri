{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmRideReq, buildOnConfirmBusReq) where

import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common

buildOnConfirmRideReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnConfirm.OnConfirmReq ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmRideReq req = do
  validateContext Context.ON_CONFIRM req.context
  handleError req.contents $ \message -> do
    return $
      DOnConfirm.OnConfirmReq
        { bppBookingId = Just $ Id message.order.id,
          bppTicketId = Nothing,
          specialZoneOtp = case message.order.fulfillment.start.authorization of
            Nothing -> Nothing
            Just auth -> Just $ auth.token
        }

buildOnConfirmBusReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnConfirm.OnConfirmReq ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmBusReq req = do
  validateBusContext Context.ON_CONFIRM req.context
  handleError req.contents $ \message -> do
    return $
      DOnConfirm.OnConfirmReq
        { bppTicketId = Just $ Id message.order.id,
          bppBookingId = Nothing,
          specialZoneOtp = Nothing
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
