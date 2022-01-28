module API.Beckn.OnSearch.Handler where

import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import qualified Core.API.OnSearch as OnSearch
import qualified Core.Context as Context
import qualified Core.DecimalValue as DecimalValue
import qualified Core.Location as Location
import qualified Core.OnSearch.Catalog as Catalog
import qualified Core.OnSearch.Item as Item
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
  logTagDebug "on_search req" (encodeToText req)
  validateContext Context.ON_SEARCH $ req.context
  Metrics.finishSearchMetrics $ req.context.transaction_id
  case req.contents of
    Right msg -> searchCbService req msg.catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

searchCbService :: EsqDBFlow m r => BecknCallbackReq OnSearch.OnSearchCatalog -> Catalog.Catalog -> m ()
searchCbService req catalog = do
  let searchRequestId = Id $ req.context.transaction_id
  _searchRequest <- QSearch.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  bppUrl <- req.context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_url")
  bppId <- req.context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  let providers = catalog.bpp_providers
  when (null providers) $ throwError $ InvalidRequest "Missing bpp_provider"
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
  let parkingSpaceName = item.descriptor.name
  let availableSpaces = item.quantity.available.count
  let bppItemId = item.id
  fare <-
    DecimalValue.convertDecimalValueToAmount item.price.value
      & fromMaybeM (InvalidRequest "Unable to parse price")
  parkingLocation <-
    find (\pl -> pl.idFromBpp == item.location_id) parkingLocations
      & fromMaybeM (InvalidRequest "Invalid item.location_id")
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
