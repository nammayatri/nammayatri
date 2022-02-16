module Product.OnSearch where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Quote as Domain
import Domain.Types.TransportStation as DTransportStation
import Storage.Queries.Quote as QQuote
import Storage.Queries.TransportStation as QTransportStation
import Tools.Error
import qualified Types.Domain.Incoming.OnSearch as DOnSearch

onSearchhandler :: EsqDBFlow m r => [DOnSearch.TransportStation] -> [DOnSearch.Quote] -> m ()
onSearchhandler transportLocations quotes = do
  publicTransportStations <- forM transportLocations $ \publicTransportStation -> do
    QTransportStation.findByStationCode publicTransportStation.bppLocationId >>= maybe (createpublicTransportLocation publicTransportStation) return
  _quotes <- forM quotes $ \quote -> do
    buildQuote publicTransportStations quote
  Esq.runTransaction $ do
    traverse_ QQuote.create _quotes

buildQuote :: (MonadGuid m, Log m, MonadThrow m) => [DTransportStation.TransportStation] -> DOnSearch.Quote -> m Domain.Quote
buildQuote transportStations quote = do
  departureStation <-
    find (\pl -> pl.stationCode == quote.bppDepartureLocId) transportStations
      & fromMaybeM (InvalidRequest "Invalid departure station code")
  arrivalStation <-
    find (\pl -> pl.stationCode == quote.bppArrivalLocId) transportStations
      & fromMaybeM (InvalidRequest "Invalid arrival station code")
  quoteId <- generateGUID
  return
    Domain.Quote
      { id = quoteId,
        searchId = quote.txnId,
        bppId = quote.bppId,
        bppUrl = quote.bppUrl,
        fare = quote.fare,
        departureTime = quote.departureTime,
        arrivalTime = quote.arrivalTime,
        createdAt = quote.createdAt,
        departureStationId = departureStation.id,
        arrivalStationId = arrivalStation.id,
        description = "",
        routeCode = quote.routeCode
      }

createpublicTransportLocation :: EsqDBFlow m r => DOnSearch.TransportStation -> m DTransportStation.TransportStation
createpublicTransportLocation publicTransportLocation = do
  publicTransportStation <- buildPublicTransportLocation publicTransportLocation
  Esq.runTransaction $ QTransportStation.create publicTransportStation
  pure publicTransportStation

buildPublicTransportLocation :: MonadGuid m => DOnSearch.TransportStation -> m DTransportStation.TransportStation
buildPublicTransportLocation publicTransportLocation = do
  id <- generateGUID
  return $
    DTransportStation.TransportStation
      { id = Id id,
        lat = publicTransportLocation.lat,
        lon = publicTransportLocation.lon,
        name = publicTransportLocation.name,
        stationCode = publicTransportLocation.bppLocationId
      }
