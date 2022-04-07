module Domain.Action.Beckn.OnSearch where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList as Kafka
import Beckn.Streaming.MonadProducer
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Search as DSearch
import qualified Domain.Types.TransportStation as DStation
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Search as QSearch
import qualified Storage.Queries.TransportStation as QStation
import Tools.Error

data OnSearchReq = OnSearchReq
  { searchId :: Id DSearch.Search,
    quotes :: [OnSearchQuoteReq],
    stations :: [OnSearchStationReq]
  }

data OnSearchQuoteReq = OnSearchQuoteReq
  { txnId :: Id DSearch.Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    bppDepartureLocId :: Text,
    bppArrivalLocId :: Text,
    createdAt :: UTCTime,
    routeCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data OnSearchStationReq = OnSearchStationReq
  { name :: Text,
    bppLocationId :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic)

handler :: (EsqDBFlow m r, MonadProducer Kafka.PublicTransportQuoteList m) => OnSearchReq -> m ()
handler (OnSearchReq searchId quotes transportLocations) = do
  searchRequest <- QSearch.findById searchId >>= fromMaybeM (SearchRequestDoesNotExist searchId.getId)
  publicTransportStations <- forM transportLocations $ \publicTransportStation -> do
    QStation.findByStationCode publicTransportStation.bppLocationId >>= maybe (createPublicTransportLocation publicTransportStation) return
  _quotes <- forM quotes $ \quote -> do
    buildQuote publicTransportStations quote
  Esq.runTransaction $ do
    traverse_ QQuote.create _quotes
  quoteAggregates <- QQuote.findAllAggregatesBySearchId searchRequest.id
  sendToKafka searchRequest.id quoteAggregates

sendToKafka :: MonadProducer Kafka.PublicTransportQuoteList m => Id DSearch.Search -> [(DQuote.Quote, DStation.TransportStation, DStation.TransportStation)] -> m ()
sendToKafka (Id txnId) quoteAggregate = Kafka.producePublicTransportQuoteListMessage txnId $ makeKafkaPublicTransportQuote <$> quoteAggregate
  where
    makeKafkaPublicTransportQuote (DQuote.Quote {..}, depStation, arrStation) =
      Kafka.PublicTransportQuote
        { id = getId id,
          departureStation = makeKafkaPublicTransportStation depStation,
          arrivalStation = makeKafkaPublicTransportStation arrStation,
          ..
        }
    makeKafkaPublicTransportStation DStation.TransportStation {..} =
      Kafka.PublicTransportStation
        { ..
        }

buildQuote :: (MonadGuid m, Log m, MonadThrow m) => [DStation.TransportStation] -> OnSearchQuoteReq -> m DQuote.Quote
buildQuote transportStations quote = do
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
        description = "",
        routeCode = quote.routeCode
      }

createPublicTransportLocation :: EsqDBFlow m r => OnSearchStationReq -> m DStation.TransportStation
createPublicTransportLocation publicTransportLocation = do
  publicTransportStation <- buildPublicTransportLocation publicTransportLocation
  Esq.runTransaction $ QStation.create publicTransportStation
  pure publicTransportStation

buildPublicTransportLocation :: MonadGuid m => OnSearchStationReq -> m DStation.TransportStation
buildPublicTransportLocation publicTransportLocation = do
  id <- generateGUID
  return $
    DStation.TransportStation
      { id = Id id,
        lat = publicTransportLocation.lat,
        lon = publicTransportLocation.lon,
        name = publicTransportLocation.name,
        stationCode = publicTransportLocation.bppLocationId
      }
