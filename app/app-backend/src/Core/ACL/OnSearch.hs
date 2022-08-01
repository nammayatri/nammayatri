module Core.ACL.OnSearch where

import Beckn.Product.Validation.Context (validateContext)
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import Beckn.Types.Id
import Beckn.Utils.Logging
import Core.ACL.Common (validatePrices)
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Domain.Types.OnSearchEvent
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, unpack)
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import Types.Error
import Utils.Common

buildOnSearchReq ::
  ( HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnSearch.OnSearchMessage ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReq req = do
  validateContext Context.ON_SEARCH $ req.context
  logOnSearchEvent req
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      Just <$> searchCbService req.context catalog
    Left err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

searchCbService :: MonadFlow m => Context.Context -> OnSearch.Catalog -> m DOnSearch.DOnSearchReq
searchCbService context catalog = do
  providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
  -- do we need throw an error when we have more than one provider?
  case catalog.bpp_providers of
    [] -> throwError $ InvalidRequest "Missing bpp/providers" -- TODO: make it NonEmpty
    (provider : _) -> do
      let items = provider.items
      (estimatesInfo, quotesInfo) <- partitionEithers <$> traverse buildEstimateOrQuoteInfo items
      let providerInfo =
            DOnSearch.ProviderInfo
              { providerId = providerId,
                name = provider.descriptor.name,
                url = providerUrl,
                mobileNumber = provider.contacts,
                ridesCompleted = provider.tags.rides_completed
              }
      pure
        DOnSearch.DOnSearchReq
          { requestId = Id context.message_id,
            ..
          }

logOnSearchEvent :: EsqDBFlow m r => OnSearch.OnSearchReq -> m ()
logOnSearchEvent (BecknCallbackReq context (leftToMaybe -> mbErr)) = do
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing context.bpp_id")
  let messageId = context.message_id
  let errorType = show . (._type) <$> mbErr
  let errorCode = (.code) <$> mbErr
  let errorMessage = (.message) =<< mbErr
  runTransaction $
    OnSearchEvent.create $ OnSearchEvent {..}

buildEstimateOrQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m (Either DOnSearch.EstimateInfo DOnSearch.QuoteInfo)
buildEstimateOrQuoteInfo item = do
  let itemCode = item.descriptor.code
      vehicleVariant = castVehicleVariant itemCode.vehicleVariant
      estimatedFare = realToFrac item.price.value
      estimatedTotalFare = realToFrac item.price.offered_value
      descriptions = item.quote_terms
  validatePrices estimatedFare estimatedTotalFare
  -- if we get here, the discount >= 0, estimatedFare >= estimatedTotalFare
  let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
  case item.category_id of
    OnSearch.ONE_WAY_TRIP -> do
      quoteDetails <- DOnSearch.OneWayDetails <$> buildOneWayQuoteDetails item
      pure $ Right DOnSearch.QuoteInfo {..}
    OnSearch.RENTAL_TRIP -> do
      quoteDetails <- DOnSearch.RentalDetails <$> buildRentalQuoteDetails item
      pure $ Right DOnSearch.QuoteInfo {..}
    OnSearch.DRIVER_OFFER_ESTIMATE -> pure $ Left DOnSearch.EstimateInfo {..}
    OnSearch.DRIVER_OFFER -> throwError $ InvalidRequest "DRIVER_OFFER supported in on_select, use DRIVER_OFFER_ESTIMATE"
  where
    castVehicleVariant = \case
      OnSearch.SEDAN -> VehVar.SEDAN
      OnSearch.SUV -> VehVar.SUV
      OnSearch.HATCHBACK -> VehVar.HATCHBACK
      OnSearch.AUTO -> VehVar.AUTO

buildOneWayQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.OneWayQuoteDetails
buildOneWayQuoteDetails item = do
  distanceToNearestDriver <-
    (item.tags <&> (.distance_to_nearest_driver))
      & fromMaybeM (InvalidRequest "Trip type is ONE_WAY, but distanceToNearestDriver is Nothing")
  pure
    DOnSearch.OneWayQuoteDetails
      { distanceToNearestDriver = realToFrac distanceToNearestDriver
      }

--FIXME remove round by using Kilometers and Hours in spec
buildRentalQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.RentalQuoteDetails
buildRentalQuoteDetails item = do
  baseDistance <- item.base_distance & fromMaybeM (InvalidRequest "Missing base_distance in rental search item")
  baseDuration <- item.base_duration & fromMaybeM (InvalidRequest "Missing base_duration in rental search item")
  pure DOnSearch.RentalQuoteDetails {..}
