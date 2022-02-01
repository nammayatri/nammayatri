module Core.ACL.Handler.OnSearch where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Types
import Beckn.Streaming.MonadProducer
import Beckn.Tools.Metrics.Types
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.ACL.Types.API.OnSearch as OnSearch
import Core.Spec.Common.Context as Context
import Core.Spec.Common.DecimalValue as DecimalValue
import Core.Spec.Common.Item as Item
import qualified Core.Spec.Common.Location as Location
import qualified Core.Spec.OnSearch.Catalog as Catalog
import Core.Spec.OnSearch.Provider
import qualified Domain.Quote as DQuote
import Domain.Search as DSearch
import Product.OnSearch as OnSearch
import Tools.Context (validateContext)
import qualified Tools.Metrics as Metrics
import Types.Domain.Incoming.OnSearch as DOnSearch

publicTransportOnSearch ::
  ( HasBAPMetrics m r,
    HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text],
    EsqDBFlow m r,
    MonadProducer BusinessEvent m,
    HasKafkaBE r kafkaEnvs
  ) =>
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  m AckResponse
publicTransportOnSearch req = do
  validateContext Context.ON_SEARCH $ req.context
  Metrics.finishSearchMetrics $ req.context.transaction_id
  case req.contents of
    Right msg -> searchCbService req msg.catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

searchCbService ::
  (EsqDBFlow m r, MonadProducer BusinessEvent m, HasKafkaBE r kafkaEnvs, ToJSON DOnSearch.Quote, ToJSON DQuote.Quote) =>
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  Catalog.Catalog ->
  m ()
searchCbService req catalog = do
  let txnId = Id $ req.context.transaction_id
  _searchRequest <- OnSearch.findSearchRequestExists txnId
  bppUrl <- req.context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_url")
  bppId <- req.context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  let providers = catalog.bpp_providers
  when (null providers) $ throwError $ InvalidRequest "Missing bpp_provider"
  now <- getCurrentTime
  publicTransportStations <-
    concat <$> forM providers \provider ->
      forM provider.locations buildPublicTransportStation
  quotes <- do
    concat <$> forM providers \provider ->
      forM provider.items (buildQuote now txnId bppUrl bppId publicTransportStations provider)
  OnSearch.onSearchhandler publicTransportStations quotes

-- quoteList <- PQuotes.getQuotesHandler txnId
-- sendToKafka quoteList

-- sendToKafka :: Quotes.GetQuotesRes -> m ()
-- sendToKafka _ = error "not implemented"

buildQuote ::
  MonadFlow m =>
  UTCTime ->
  Id DSearch.Search ->
  BaseUrl ->
  Text ->
  [DOnSearch.PublicTranport] ->
  Provider ->
  Item.Item ->
  m DOnSearch.Quote
buildQuote now txnId bppUrl bppId publicTransportLocations provider item = do
  let departureId = item.departure_id
  let fareId = item.fare_id
  let fareList = provider.fares
  let departureList = provider.departures
  let routeList = provider.routes

  fares <-
    find (\pl -> pl.id == fareId) fareList
      & fromMaybeM (InvalidRequest "Invalid provider.fares")
  fare <-
    DecimalValue.convertDecimalValueToAmount fares.price.value
      & fromMaybeM (InvalidRequest "Unable to parse price")
  departures <-
    find (\pl -> pl.id == departureId) departureList
      & fromMaybeM (InvalidRequest "Invalid provider.departures")
  routes <-
    find (\pl -> pl.id == departures.route_id) routeList
      & fromMaybeM (InvalidRequest "Invalid provider.routes")
  departureLocation <-
    find (\pl -> pl.bppLocationId == routes.start_stop) publicTransportLocations
      & fromMaybeM (InvalidRequest "Invalid item.start_location")
  arrivalLocation <-
    find (\pl -> pl.bppLocationId == routes.end_stop) publicTransportLocations
      & fromMaybeM (InvalidRequest "Invalid item.end_location")
  return
    DOnSearch.Quote
      { txnId = txnId,
        bppId = bppId,
        bppUrl = bppUrl,
        fare = fare,
        departureTime = departures.start_time.timestamp,
        arrivalTime = departures.end_time.timestamp,
        createdAt = now,
        bppDepartureLocId = departureLocation.bppLocationId,
        bppArrivalLocId = arrivalLocation.bppLocationId,
        description = ""
      }

buildPublicTransportStation :: MonadGuid m => Location.Location -> m DOnSearch.PublicTranport
buildPublicTransportStation location = do
  return
    DOnSearch.PublicTranport
      { lat = location.gps.lat,
        lon = location.gps.lon,
        name = location.descriptor.name,
        bppLocationId = location.id
      }