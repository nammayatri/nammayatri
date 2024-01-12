{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit (buildOnInitReq) where

import Beckn.ACL.Common
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.OnInit as DOnInit
import Domain.Types.Booking (BPPBooking, Booking)
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common

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

_buildOnInitReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text]
  ) =>
  Spec.OnInitReq ->
  m (Maybe DOnInit.OnInitReq)
_buildOnInitReqV2 req = do
  ContextV2.validateContext Context.ON_INIT $ req.onInitReqContext
  handleErrorV2 req $ \_message ->
    case parsedData of
      Left err -> do
        logTagError "on_init req" $ "on_init error: " <> show err
        pure Nothing
      Right (bookingId, bppBookingId, estimatedFare, estimatedTotalFare) -> do
        validatePrices estimatedFare estimatedTotalFare
        -- if we get here, the discount >= 0
        let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
        return $
          Just $
            DOnInit.OnInitReq
              { estimatedFare = Money estimatedFare,
                estimatedTotalFare = Money estimatedTotalFare,
                discount = Money <$> discount,
                paymentUrl = Nothing, -- TODO check with ONDC
                ..
              }
  where
    parsedData :: Either Text (Id Booking, Id BPPBooking, Int, Int)
    parsedData = do
      order <- req.onInitReqMessage <&> (.confirmReqMessageOrder) & maybe (Left "Invalid Order") Right

      bookingIdText <-
        (fmap UUID.toText req.onInitReqContext.contextMessageId)
          & maybe (Left "Invalid messageId") Right
      let bookingId = Id bookingIdText

      bppBookingIdText <-
        order.orderId
          & maybe (Left "Invalid OrderId") Right
      let bppBookingId = Id bppBookingIdText

      estimatedFare <-
        order.orderQuote
          >>= (.quotationPrice)
          >>= (.priceValue)
          >>= parseInt
          & maybe (Left "Invalid Price") Right

      estimatedTotalFare <-
        order.orderQuote
          >>= (.quotationPrice)
          >>= (.priceOfferedValue)
          >>= parseInt
          & maybe (Left "Invalid Offered Price") Right

      Right (bookingId, bppBookingId, estimatedFare, estimatedTotalFare)

    parseInt :: Text -> Maybe Int
    parseInt = readMaybe . T.unpack

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnInitReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnInit.OnInitReq)) ->
  m (Maybe DOnInit.OnInitReq)
handleErrorV2 req action =
  case req.onInitReqError of
    Nothing -> req.onInitReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_init req" $ "on_init error: " <> show err
      pure Nothing
