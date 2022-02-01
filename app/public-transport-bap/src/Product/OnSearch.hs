module Product.OnSearch where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.PublicTranport as DPublicTransport
import Domain.Quote as DQuote
import Domain.Search
import Storage.Queries.PublicTranport as QPublicTransport
import Storage.Queries.Quote as QQuote
import Storage.Queries.Search as QSearch
import Tools.Error
import qualified Types.Domain.Incoming.OnSearch as DOnSearch

findSearchRequestExists :: EsqDBFlow m r => Id Search -> m Search
findSearchRequestExists txnId = do
  QSearch.findById txnId >>= fromMaybeM SearchRequestDoesNotExist

onSearchhandler :: EsqDBFlow m r => [DOnSearch.PublicTranport] -> [DOnSearch.Quote] -> m ()
onSearchhandler transportLocations quotes = do
  publicTransportStations <- forM transportLocations $ \publicTransportStation -> do
    QPublicTransport.findByStationCode publicTransportStation.bppLocationId >>= maybe (createpublicTransportLocation publicTransportStation) return
  _quotes <- forM quotes $ \quote -> do
    makeQuote publicTransportStations quote
  Esq.runTransaction $ do
    traverse_ QQuote.create _quotes

makeQuote :: (MonadGuid m, Log m, MonadThrow m) => [DPublicTransport.PublicTranport] -> DOnSearch.Quote -> m DQuote.Quote
makeQuote transportStations quote = do
  departureStation <-
    find (\pl -> pl.stationCode == quote.bppDepartureLocId) transportStations
      & fromMaybeM (InvalidRequest "Invalid departure station code")
  arrivalStation <-
    find (\pl -> pl.stationCode == quote.bppArrivalLocId) transportStations
      & fromMaybeM (InvalidRequest "Invalid arrival station code")
  quoteId <- generateGUID
  return
    DQuote.Quote
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
        description = ""
      }

createpublicTransportLocation :: EsqDBFlow m r => DOnSearch.PublicTranport -> m DPublicTransport.PublicTranport
createpublicTransportLocation publicTransportLocation = do
  publicTransportStation <- makePublicTransportLocation publicTransportLocation
  Esq.runTransaction $ QPublicTransport.create publicTransportStation
  pure publicTransportStation

makePublicTransportLocation :: MonadGuid m => DOnSearch.PublicTranport -> m DPublicTransport.PublicTranport
makePublicTransportLocation publicTransportLocation = do
  id <- generateGUID
  return $
    DPublicTransport.PublicTranport
      { id = Id id,
        lat = publicTransportLocation.lat,
        lon = publicTransportLocation.lon,
        name = publicTransportLocation.name,
        stationCode = publicTransportLocation.bppLocationId
      }