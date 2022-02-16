module Core.ACL.Handler.OnSearch where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Streaming.MonadProducer
import Beckn.Tools.Metrics.Types
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.ACL.Types.API.OnSearch as OnSearch
import Core.Spec.Common.Context as Context
import Core.Spec.Common.Item as Item
import qualified Core.Spec.Common.Location as Location
import qualified Core.Spec.OnSearch.Catalog as Catalog
import Core.Spec.OnSearch.Provider
import qualified Domain.Types.Quote as Domain
import Domain.Types.Search as Domain
import qualified Domain.Types.TransportStation as Domain
import qualified Product.OnSearch as OnSearch
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Search as QSearch
import Tools.Context (validateContext)
import Types.Domain.Incoming.OnSearch as DOnSearch

publicTransportOnSearch ::
  ( HasBAPMetrics m r,
    EsqDBFlow m r,
    MonadProducer PublicTransportQuoteList m,
    HasKafkaBE r kafkaEnvs
  ) =>
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  m AckResponse
publicTransportOnSearch req = do
  validateContext Context.ON_SEARCH $ req.context
  case req.contents of
    Right msg -> searchCbService req msg.catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

searchCbService ::
  (EsqDBFlow m r, MonadProducer PublicTransportQuoteList m, ToJSON DOnSearch.Quote, ToJSON Domain.Quote) =>
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  Catalog.Catalog ->
  m ()
searchCbService req catalog = do
  let txnId = Id $ req.context.transaction_id
  searchRequest <- QSearch.findById txnId >>= fromMaybeM SearchRequestDoesNotExist
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
  quoteAggregates <- QQuote.findAllAggregatesBySearchId searchRequest.id
  sendToKafka searchRequest.id quoteAggregates

sendToKafka :: MonadProducer PublicTransportQuoteList m => Id Domain.Search -> [(Domain.Quote, Domain.TransportStation, Domain.TransportStation)] -> m ()
sendToKafka (Id txnId) quoteAggregate = producePublicTransportQuoteListMessage txnId $ makeKafkaPublicTransportQuote <$> quoteAggregate
  where
    makeKafkaPublicTransportQuote (Domain.Quote {..}, depStation, arrStation) =
      PublicTransportQuote
        { id = getId id,
          departureStation = makeKafkaPublicTransportStation depStation,
          arrivalStation = makeKafkaPublicTransportStation arrStation,
          ..
        }
    makeKafkaPublicTransportStation Domain.TransportStation {..} =
      PublicTransportStation
        { ..
        }

buildQuote ::
  MonadFlow m =>
  UTCTime ->
  Id Domain.Search ->
  BaseUrl ->
  Text ->
  [DOnSearch.TransportStation] ->
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
  let fare = fares.price.value
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
        description = "",
        routeCode = routes.route_code
      }

buildPublicTransportStation :: MonadGuid m => Location.Location -> m DOnSearch.TransportStation
buildPublicTransportStation location = do
  return
    DOnSearch.TransportStation
      { lat = location.gps.lat,
        lon = location.gps.lon,
        name = location.descriptor.name,
        bppLocationId = location.id
      }
