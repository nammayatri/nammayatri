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
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Domain.Types.Booking (BPPBooking)
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common

buildOnConfirmReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
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

buildOnConfirmReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text]
  ) =>
  Spec.OnConfirmReq ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmReqV2 req = do
  ContextV2.validateContext Context.ON_CONFIRM req.onConfirmReqContext
  handleErrorV2 req $ \message -> do
    case parseData message of
      Left err -> do
        logTagError "on_init req" $ "on_init error: " <> show err
        return Nothing
      Right (bppBookingId, mAuthorization) -> do
        return $
          Just $
            DOnConfirm.OnConfirmReq
              { bppBookingId,
                specialZoneOtp = mAuthorization
              }
  where
    parseData :: Spec.ConfirmReqMessage -> Either Text (Id BPPBooking, Maybe Text)
    parseData message = do
      let order = message.confirmReqMessageOrder

      bppBookingIdText <-
        order.orderId
          & maybe (Left "Invalid OrderId") Right
      let bppBookingId = Id bppBookingIdText

      let startOtp =
            order.orderFulfillments
              >>= listToMaybe
              >>= (.fulfillmentStops)
              >>= Utils.getStartLocation
              >>= (.stopAuthorization)
              >>= \auth -> if auth.authorizationType == Just "OTP" then auth.authorizationToken else Nothing

      return (bppBookingId, startOtp)

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnConfirmReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnConfirm.OnConfirmReq)) ->
  m (Maybe DOnConfirm.OnConfirmReq)
handleErrorV2 req action =
  case req.onConfirmReqError of
    Nothing -> req.onConfirmReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_confirm req" $ "on_confirm error: " <> show err
      pure Nothing
