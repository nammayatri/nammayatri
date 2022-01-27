module Product.Quote where

import App.Types
import Beckn.Storage.Hedis as Hedis
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.SearchReqLocation as Location
import qualified Domain.Types.SearchRequest as SSR
import EulerHS.Prelude hiding (id)
import qualified Product.MetroOffer as Metro
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Storage.Queries.SearchRequest as QSR
import Types.API.MetroOffer (MetroOffer (..))
import qualified Types.API.Quote as API
import Types.Error
import Utils.Common

getQuotes :: Id SSR.SearchRequest -> Id Person.Person -> FlowHandler API.GetQuotesRes
getQuotes searchRequestId _ = withFlowHandlerAPI $ do
  searchRequest <- QSR.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  fromLocation <- Location.findById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- Location.findById searchRequest.toLocationId >>= fromMaybeM LocationNotFound
  offers <- getOffers searchRequest
  return $
    API.GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity toLocation,
        quotes = offers
      }

getOffers :: (HedisFlow m r, EsqDBFlow m r) => SSR.SearchRequest -> m [API.OfferRes]
getOffers searchRequest = do
  quoteList <- QQuote.findAllByRequestId searchRequest.id
  let quotes = API.OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByNearestDriverDistance quoteList
  metroOffers <- map API.Metro <$> Metro.getMetroOffers searchRequest.id
  publicTransportOffers <- map API.PublicTransport <$> getPubTransportOffers searchRequest.id
  return . sortBy (compare `on` creationTime) $ quotes <> metroOffers <> publicTransportOffers
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = compare `on` (.distanceToNearestDriver)
      sortBy sortFunc quoteList
    creationTime :: API.OfferRes -> UTCTime
    creationTime (API.OnDemandCab SQuote.QuoteAPIEntity {createdAt}) = createdAt
    creationTime (API.Metro MetroOffer {createdAt}) = createdAt
    creationTime (API.PublicTransport PublicTransportQuote {createdAt}) = createdAt

getPubTransportOffers :: (HedisFlow m r, MonadFlow m) => Id SSR.SearchRequest -> m [PublicTransportQuote]
getPubTransportOffers transactionId =
  Hedis.getList redisKey
  where
    redisKey = "publicTransportQuoteList:" <> getId transactionId
