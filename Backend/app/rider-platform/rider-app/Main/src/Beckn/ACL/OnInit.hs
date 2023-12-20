{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit (buildOnInitReq, buildOnInitReqV2) where

import Beckn.ACL.Common
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
import qualified Domain.Action.Beckn.OnInit as DOnInit
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

buildOnInitReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnInit.OnInitReq ->
  m (Maybe DOnInit.OnInitReq)
buildOnInitReq req = do
  validateContext Context.ON_INIT $ req.context
  handleError req.contents $ \message -> do
    let bookingId = Id req.context.message_id
        bppBookingId = Id message.order.id
        estimatedFare = message.order.quote.price.value
        estimatedTotalFare = message.order.quote.price.offered_value
    validatePrices estimatedFare estimatedTotalFare
    -- if we get here, the discount >= 0
    let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
    return $
      DOnInit.OnInitReq
        { estimatedFare = roundToIntegral estimatedFare,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          discount = roundToIntegral <$> discount,
          paymentUrl = message.order.payment.uri,
          ..
        }

buildOnInitReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text]
  ) =>
  OnInit.OnInitReqV2 ->
  m (Maybe DOnInit.OnInitReq)
buildOnInitReqV2 req = do
  validateContextV2 Context.ON_INIT $ req.context
  handleErrorV2 req.contents $ \message -> do
    let bookingId = Id req.context.message_id
        bppBookingId = Id message.order.id
        estimatedFare = message.order.quote.price.value
        estimatedTotalFare = message.order.quote.price.offered_value
    validatePrices estimatedFare estimatedTotalFare
    -- if we get here, the discount >= 0
    let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
    payment <- case message.order.payments of
      [payment] -> pure payment
      _ -> throwError . InvalidRequest $ "There must be exactly one payment in on_init request " <> show message.order.payments
    return $
      DOnInit.OnInitReq
        { estimatedFare = roundToIntegral estimatedFare,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          discount = roundToIntegral <$> discount,
          paymentUrl = payment.uri,
          ..
        }

handleError ::
  (MonadFlow m) =>
  Either Error OnInit.OnInitMessage ->
  (OnInit.OnInitMessage -> m DOnInit.OnInitReq) ->
  m (Maybe DOnInit.OnInitReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_init req" $ "on_init error: " <> show err
      pure Nothing

handleErrorV2 ::
  (MonadFlow m) =>
  Either Error OnInit.OnInitMessageV2 ->
  (OnInit.OnInitMessageV2 -> m DOnInit.OnInitReq) ->
  m (Maybe DOnInit.OnInitReq)
handleErrorV2 etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_init req" $ "on_init error: " <> show err
      pure Nothing
