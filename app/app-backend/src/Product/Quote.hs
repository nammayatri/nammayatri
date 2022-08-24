module Product.Quote where

import App.Types
import Beckn.Storage.Hedis as Hedis
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Types.Id
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.SearchRequest as SSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as Location
import EulerHS.Prelude hiding (id)
import qualified Product.MetroOffer as Metro
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Quote as QRentalQuote
import qualified Storage.Queries.SearchRequest as QSR
import Types.API.MetroOffer (MetroOffer (..))
import qualified Types.API.Quote as API
import Types.Error
import Utils.Common

getQuotes :: Id SSR.SearchRequest -> Id Person.Person -> FlowHandler API.GetQuotesRes
getQuotes searchRequestId _ = withFlowHandlerAPI $ do
  searchRequest <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  offers <- getOffers searchRequest
  estimates <- getEstimates searchRequestId
  return $
    API.GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity searchRequest.fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity <$> searchRequest.toLocation,
        quotes = offers,
        estimates
      }

getOffers :: (HedisFlow m r, EsqDBFlow m r) => SSR.SearchRequest -> m [API.OfferRes]
getOffers searchRequest = do
  -- ONE_WAY and RENTAL cases
  case searchRequest.toLocation of
    Just _ -> do
      quoteList <- QQuote.findAllByRequestId searchRequest.id
      let quotes = API.OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByNearestDriverDistance quoteList
      metroOffers <- map API.Metro <$> Metro.getMetroOffers searchRequest.id
      publicTransportOffers <- map API.PublicTransport <$> getPubTransportOffers searchRequest.id
      return . sortBy (compare `on` creationTime) $ quotes <> metroOffers <> publicTransportOffers
    Nothing -> do
      quoteList <- QRentalQuote.findAllByRequestId searchRequest.id
      let quotes = API.OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByEstimatedFare quoteList
      return . sortBy (compare `on` creationTime) $ quotes
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = compare `on` getMbDistanceToNearestDriver
      sortBy sortFunc quoteList
    getMbDistanceToNearestDriver quote = do
      case quote.quoteDetails of
        SQuote.OneWayDetails details -> Just details.distanceToNearestDriver
        SQuote.RentalDetails _ -> Nothing
        SQuote.DriverOfferDetails details -> Just details.distanceToPickup
    creationTime :: API.OfferRes -> UTCTime
    creationTime (API.OnDemandCab SQuote.QuoteAPIEntity {createdAt}) = createdAt
    creationTime (API.Metro MetroOffer {createdAt}) = createdAt
    creationTime (API.PublicTransport PublicTransportQuote {createdAt}) = createdAt

getPubTransportOffers :: (HedisFlow m r, MonadFlow m) => Id SSR.SearchRequest -> m [PublicTransportQuote]
getPubTransportOffers transactionId =
  Hedis.getList redisKey
  where
    redisKey = "publicTransportQuoteList:" <> getId transactionId

getEstimates :: EsqDBFlow m r => Id SSR.SearchRequest -> m [DEstimate.EstimateAPIEntity]
getEstimates searchRequestId = do
  estimateList <- QEstimate.findAllByRequestId searchRequestId
  let estimates = DEstimate.mkEstimateAPIEntity <$> sortByEstimatedFare estimateList
  return . sortBy (compare `on` (.createdAt)) $ estimates

sortByEstimatedFare :: (HasField "estimatedFare" r Money) => [r] -> [r]
sortByEstimatedFare resultList = do
  let sortFunc = compare `on` (.estimatedFare)
  sortBy sortFunc resultList
