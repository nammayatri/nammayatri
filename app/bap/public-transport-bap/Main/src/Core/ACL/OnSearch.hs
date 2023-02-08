module Core.ACL.OnSearch where

import qualified Core.Spec.OnSearch as OnSearch
import Core.Spec.OnSearch.Provider
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Domain.Types.Search as Domain
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

buildOnSearch ::
  MonadFlow m =>
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  OnSearch.Catalog ->
  m DOnSearch.OnSearchReq
buildOnSearch req catalog = do
  txnId <- Id <$> req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  bppUrl <- req.context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_url")
  bppId <- req.context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  let providers = catalog.bpp_providers
  when (null providers) $ throwError $ InvalidRequest "Missing bpp_provider"
  now <- getCurrentTime
  publicTransportStations <-
    concat <$> forM providers \provider ->
      forM provider.locations (pure . mkPublicTransportStation)
  quotes <- do
    concat <$> forM providers \provider ->
      forM provider.items (buildQuote now txnId bppUrl bppId publicTransportStations provider)
  -- FIXME we do not need to duplicate the same data in each quote
  pure $ DOnSearch.OnSearchReq txnId quotes publicTransportStations

buildQuote ::
  MonadFlow m =>
  UTCTime ->
  Id Domain.Search ->
  BaseUrl ->
  Text ->
  [DOnSearch.OnSearchStationReq] ->
  Provider ->
  OnSearch.Item ->
  m DOnSearch.OnSearchQuoteReq
buildQuote now txnId bppUrl bppId publicTransportLocations provider item = do
  let departureId = item.departure_id
  let fareId = item.fare_id
  let fareList = provider.fares
  let departureList = provider.departures
  let routeList = provider.routes

  fares <-
    find (\pl -> pl.id == fareId) fareList
      & fromMaybeM (InvalidRequest "Invalid provider.fares")
  let fare = roundToIntegral fares.price.value
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
    DOnSearch.OnSearchQuoteReq
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

mkPublicTransportStation :: OnSearch.LocationDetails -> DOnSearch.OnSearchStationReq
mkPublicTransportStation location = do
  DOnSearch.OnSearchStationReq
    { lat = location.gps.lat,
      lon = location.gps.lon,
      name = location.descriptor.name,
      bppLocationId = location.id
    }
