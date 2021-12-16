module API.Beckn.OnSearch.Handler where

import qualified API.Beckn.OnSearch.Types as OnSearch
import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Core.API.Types (BecknCallbackReq)
import qualified Core.Context as Context
import qualified Core.DecimalValue as DecimalValue
import qualified Core.Item as Item
import qualified Core.OnSearch.Catalog as Catalog
import qualified Core.OnSearch.Location as Location
import qualified Domain.ParkingLocation as DParkingLocation
import qualified Domain.Quote as DQuote
import qualified Domain.Search as DSearch
import qualified Storage.Queries.ParkingLocation as QParkingLocation
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Search as QSearch
import Tools.Context (validateContext)
import qualified Tools.Metrics as Metrics

handler ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  FlowHandler AckResponse
handler _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_SEARCH $ req.context
  Metrics.finishSearchMetrics $ req.context.transaction_id
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      searchCbService req catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

searchCbService :: EsqDBFlow m r => BecknCallbackReq OnSearch.OnSearchCatalog -> Catalog.Catalog -> m ()
searchCbService req catalog = do
  let searchRequestId = Id $ req.context.transaction_id
  _searchRequest <- QSearch.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  bppUrl <- maybe (throwError $ InvalidRequest "Missing bpp url") pure req.context.bpp_uri
  bppId <- maybe (throwError $ InvalidRequest "Missing bpp id") pure req.context.bpp_id
  let providers = catalog.bpp_providers
  when (null providers) $ throwError $ InvalidRequest "Missing provider"
  now <- getCurrentTime
  parkingLocations <- do
    allParkingLocationsByProvider <- forM providers $ \provider -> do
      forM provider.locations (buildParkingLocation now)
    return $ concat allParkingLocationsByProvider
  quotes <- do
    allQuotesByProvider <- forM providers $ \provider -> do
      let items = fromMaybe [] provider.items
      forM items (buildQuote now searchRequestId bppUrl bppId parkingLocations)
    return $ concat allQuotesByProvider
  Esq.runTransaction $ do
    traverse_ QParkingLocation.create parkingLocations
    traverse_ QQuote.create quotes

buildQuote ::
  MonadFlow m =>
  UTCTime ->
  Id DSearch.Search ->
  BaseUrl ->
  Text ->
  [DParkingLocation.ParkingLocation] ->
  Item.Item ->
  m DQuote.Quote
buildQuote now searchId bppUrl bppId parkingLocations item = do
  decimalValue <-
    item.price.value
      & fromMaybeM (InvalidRequest "Unable to parse price")
  fare <-
    decimalValue
      & DecimalValue.convertDecimalValueToAmount
      & fromMaybeM (InvalidRequest "Unable to parse price")
  parkingSpaceName <-
    item.descriptor.name
      & fromMaybeM (InvalidRequest "Unable to parse parking space name")
  availableSpaces <-
    item.quantity.available >>= (.count) <&> fromInteger
      & fromMaybeM (InvalidRequest "Unable to parse available spaces")
  parkingLocation <-
    find (\pl -> pl.idFromBpp == item.location_id) parkingLocations
      & fromMaybeM (InvalidRequest "Unable to parse parking location id")
  bppItemId <-
    item.id & fromMaybeM (InvalidRequest "Parking space id is not present.")
  quoteId <- generateGUID
  return
    DQuote.Quote
      { id = quoteId,
        parkingLocationId = parkingLocation.id,
        parkingLocationIdFromBpp = item.location_id,
        createdAt = now,
        ..
      }

buildParkingLocation :: MonadGuid m => UTCTime -> Location.Location -> m DParkingLocation.ParkingLocation
buildParkingLocation now location = do
  id <- generateGUID
  return
    DParkingLocation.ParkingLocation
      { id = Id id,
        idFromBpp = location.id,
        lat = location.gps.lat,
        lon = location.gps.lon,
        name = location.address.name,
        country = location.address.country,
        city = location.address.city,
        state = location.address.state,
        locality = location.address.locality,
        areaCode = location.address.area_code,
        streetAddress = location.address.street_address,
        createdAt = now
      }
