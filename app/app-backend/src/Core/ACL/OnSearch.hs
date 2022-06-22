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
import Data.Text (unpack)
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Domain.Types.OnSearchEvent
import qualified Domain.Types.Quote as DQuote
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
      fareProductType <-
        readMaybe (unpack provider.category_id)
          & fromMaybeM (InvalidRequest "Invalid category_id. Only RENTAL and ONE_WAY is supported by BAP")
      quotesInfo <- traverse (buildQuoteInfo fareProductType) items
      let providerInfo =
            DOnSearch.ProviderInfo
              { providerId = providerId,
                name = provider.name,
                url = providerUrl,
                mobileNumber = provider.contacts,
                ridesCompleted = provider.rides_completed
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

buildQuoteInfo ::
  (MonadThrow m, Log m) =>
  DQuote.FareProductType ->
  OnSearch.Item ->
  m DOnSearch.QuoteInfo
buildQuoteInfo fareProductType item = do
  quoteDetails <- case fareProductType of
    DQuote.ONE_WAY -> DQuote.OneWayDetails <$> buildOneWayQuoteDetails item
    DQuote.RENTAL -> DQuote.RentalDetails <$> buildRentalQuoteDetails item
  pure
    DOnSearch.QuoteInfo
      { bppQuoteId = Id item.id,
        vehicleVariant = item.vehicle_variant,
        estimatedFare = realToFrac item.estimated_price.value,
        discount = realToFrac <$> (item.discount <&> (.value)),
        estimatedTotalFare = realToFrac item.discounted_price.value,
        descriptions = fromMaybe [] item.descriptions, --item.descriptions should not be Maybe
        ..
      }

buildOneWayQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DQuote.OneWayQuoteDetails
buildOneWayQuoteDetails item = do
  distanceToNearestDriver <- item.nearest_driver_distance & fromMaybeM (InvalidRequest "Missing nearest_driver_distance in one way search item")
  pure
    DQuote.OneWayQuoteDetails
      { distanceToNearestDriver = realToFrac distanceToNearestDriver
      }

buildRentalQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DQuote.RentalQuoteDetails
buildRentalQuoteDetails item = do
  baseDistance <- item.baseDistance & fromMaybeM (InvalidRequest "Missing baseDistance in rental search item")
  baseDurationHr <- item.baseDurationHr & fromMaybeM (InvalidRequest "Missing baseDurationHr in rental search item")

  pure DQuote.RentalQuoteDetails {..}
