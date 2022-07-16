module Core.ACL.OnSelect where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import Beckn.Types.Id
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Domain.Types.VehicleVariant
import Types.Error
import Utils.Common

buildOnSelectReq ::
  HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text] =>
  OnSelect.OnSelectReq ->
  m (Maybe DOnSelect.DOnSelectReq)
buildOnSelectReq req = do
  logDebug $ "on_select request: " <> show req
  let context = req.context
  validateContext Context.ON_SELECT context
  handleError req.contents $ \message -> do
    providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
    providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
    let provider = message.order.provider
    let items = provider.items
    quotesInfo <- traverse buildQuoteInfo items
    let providerInfo =
          DOnSelect.ProviderInfo
            { providerId = providerId,
              name = provider.descriptor.name,
              url = providerUrl,
              mobileNumber = provider.contacts,
              ridesCompleted = provider.tags.rides_completed
            }
    pure
      DOnSelect.DOnSelectReq
        { quoteId = Id context.message_id,
          ..
        }

handleError ::
  (MonadFlow m) =>
  Either Error OnSelect.OnSelectMessage ->
  (OnSelect.OnSelectMessage -> m DOnSelect.DOnSelectReq) ->
  m (Maybe DOnSelect.DOnSelectReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_select req" $ "on_select error: " <> show err
      pure Nothing

buildQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSelect.Item ->
  m DOnSelect.QuoteInfo
buildQuoteInfo item = do
  quoteDetails <- case item.category_id of
    OnSelect.ONE_WAY_TRIP -> throwError $ InvalidRequest "select not supported for one way trip"
    OnSelect.RENTAL_TRIP -> throwError $ InvalidRequest "select not supported for rental trip"
    OnSelect.AUTO_TRIP -> DOnSelect.AutoDetails <$> buildAutoQuoteDetails item
  let itemCode = item.descriptor.code
      vehicleVariant = itemCode.vehicleVariant
      estimatedFare = realToFrac item.price.value
      estimatedTotalFare = realToFrac item.price.offered_value
      descriptions = item.quote_terms
  pure
    DOnSelect.QuoteInfo
      { discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedTotalFare - estimatedFare,
        vehicleVariant = castVehicleVariant vehicleVariant,
        ..
      }
  where
    castVehicleVariant = \case
      OnSelect.SEDAN -> SEDAN
      OnSelect.SUV -> SUV
      OnSelect.HATCHBACK -> HATCHBACK
      OnSelect.AUTO -> AUTO

buildAutoQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSelect.Item ->
  m DOnSelect.AutoQuoteDetails
buildAutoQuoteDetails item = do
  driverName <- item.driver_name & fromMaybeM (InvalidRequest "Missing driver_name in auto select item")
  durationToPickup <- item.duration_to_pickup & fromMaybeM (InvalidRequest "Missing duration_to_pickup in auto select item")
  distanceToPickup' <-
    (item.tags <&> (.distance_to_nearest_driver))
      & fromMaybeM (InvalidRequest "Trip type is AUTO, but distanceToNearestDriver is Nothing")
  validTill <- item.valid_till & fromMaybeM (InvalidRequest "Missing valid_till in auto select item")
  let rating = item.rating
  let bppQuoteId = item.id
  pure $
    DOnSelect.AutoQuoteDetails
      { distanceToPickup = realToFrac distanceToPickup',
        bppDriverQuoteId = Id bppQuoteId,
        ..
      }
