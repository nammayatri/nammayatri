module Core.ACL.OnInit (buildOnInitReq) where

import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
import Core.ACL.Common
import qualified Domain.Action.Beckn.OnInit as DOnInit
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
