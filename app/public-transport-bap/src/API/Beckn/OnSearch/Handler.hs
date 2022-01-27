module API.Beckn.OnSearch.Handler where

import qualified API.Beckn.OnSearch.Types as OnSearch
import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Core.Context as Context
import Core.DecimalValue as DecimalValue
import Core.Item as Item
import qualified Core.Location as Location
import qualified Core.OnSearch.Catalog as Catalog
import Domain.FerryStation as DFerryStation
import Domain.Quote as DQuote
import Domain.Search as DSearch
import Storage.Queries.FerryStation as QFerryStation
import Storage.Queries.Quote as QQuote
import Storage.Queries.Search as QSearch
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
  ferryStations <- do
    allParkingLocationsByProvider <- forM providers $ \provider -> do
      forM provider.locations (buildFerryStation now)
    return $ concat allParkingLocationsByProvider
  quotes <- do
    allQuotesByProvider <- forM providers $ \provider -> do
      let items = provider.items
      forM items (buildQuote now searchRequestId bppUrl bppId ferryStations)
    return $ concat allQuotesByProvider
  Esq.runTransaction $ do
    traverse_ QFerryStation.create ferryStations
    traverse_ QQuote.create quotes

buildQuote ::
  MonadFlow m =>
  UTCTime ->
  Id DSearch.Search ->
  BaseUrl ->
  Text ->
  [DFerryStation.FerryStation] ->
  Item.Item ->
  m DQuote.Quote
buildQuote now searchId bppUrl bppId ferryStations item = do
  let departureTime = item.departure_time.timestamp
  let arrivalTime = item.arrival_time.timestamp
  fare <-
    DecimalValue.convertDecimalValueToAmount item.price.value
      & fromMaybeM (InvalidRequest "Unable to parse price")
  startLocation <-
    find (\pl -> pl.stationCode == item.start_location) ferryStations
      & fromMaybeM (InvalidRequest "Invalid item.start_location")
  endLocation <-
    find (\pl -> pl.stationCode == item.end_location) ferryStations
      & fromMaybeM (InvalidRequest "Invalid item.end_location")
  quoteId <- generateGUID
  return
    DQuote.Quote
      { id = quoteId,
        searchId = searchId,
        bppId = bppId,
        bppUrl = bppUrl,
        fare = fare,
        departureTime = departureTime,
        arrivalTime = arrivalTime,
        createdAt = now,
        departureStationId = startLocation.id,
        arrivalStationId = endLocation.id,
        description = ""
      }

buildFerryStation :: MonadGuid m => UTCTime -> Location.Location -> m DFerryStation.FerryStation
buildFerryStation _ location = do
  id <- generateGUID
  return
    DFerryStation.FerryStation
      { id = Id id,
        lat = location.gps.lat,
        lon = location.gps.lon,
        name = location.descriptor.name,
        stationCode = location.stop_code
      }
