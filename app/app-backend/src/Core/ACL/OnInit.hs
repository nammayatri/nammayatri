module Core.ACL.OnInit (buildOnInitReq) where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
import Beckn.Types.Id
import qualified Domain.Action.Beckn.OnInit as DOnInit
import Types.Error
import Utils.Common

buildOnInitReq ::
  ( HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]
  ) =>
  OnInit.OnInitReq ->
  m (Maybe DOnInit.OnInitReq)
buildOnInitReq req = do
  validateContext Context.ON_INIT $ req.context
  handleError req.contents $ \message -> do
    let rideBookingId = Id req.context.message_id
        bppRideBookingId = Id message.order.id
        estimatedFare = message.order.quote.price.value
        estimatedTotalFare = message.order.quote.price.offered_value
        -- We assume that first item in list is always estimatedPriceBreakup and others are discounts
        (estimatedPriceBreakup : discountPriceBreakups) = message.order.quote.breakup
    unless (estimatedFare == estimatedPriceBreakup.price.value) $
      throwError (InvalidRequest "Estimated fare is not first item in Breakup list.")
    let discount =
          if null discountPriceBreakups
            then Nothing
            else Just $ foldr (\v sm -> sm + v.price.value) 0 discountPriceBreakups
    whenJust discount $ \disc ->
      when (estimatedFare < disc) $ throwError (InvalidRequest "Discount value more than estimated fare.")
    return $
      DOnInit.OnInitReq
        { estimatedFare = realToFrac estimatedFare,
          estimatedTotalFare = realToFrac estimatedTotalFare,
          discount = realToFrac <$> discount,
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
