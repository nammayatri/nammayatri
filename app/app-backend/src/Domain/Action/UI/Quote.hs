module Domain.Action.UI.Quote
  ( GetQuotesRes (..),
    OfferRes (..),
    getQuotes,
  )
where

import Beckn.Storage.Hedis as Hedis
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Types.Id
import Beckn.Utils.JSON (objectWithSingleFieldParsing)
import qualified Beckn.Utils.Schema as S
import Data.Char (toLower)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Domain.Types.Estimate (EstimateAPIEntity)
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.Quote (QuoteAPIEntity)
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.SearchRequest as SSR
import Domain.Types.SearchRequest.SearchReqLocation (SearchReqLocationAPIEntity)
import qualified Domain.Types.SearchRequest.SearchReqLocation as Location
import EulerHS.Prelude hiding (id)
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import qualified SharedLogic.PublicTransport as PublicTransport
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Quote as QRentalQuote
import qualified Storage.Queries.SearchRequest as QSR
import Types.Error
import Utils.Common

data GetQuotesRes = GetQuotesRes
  { fromLocation :: SearchReqLocationAPIEntity,
    toLocation :: Maybe SearchReqLocationAPIEntity,
    quotes :: [OfferRes],
    estimates :: [EstimateAPIEntity]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OfferRes
  = OnDemandCab QuoteAPIEntity
  | Metro MetroOffer
  | PublicTransport PublicTransportQuote
  deriving (Show, Generic)

instance ToJSON OfferRes where
  toJSON = genericToJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance ToSchema OfferRes where
  declareNamedSchema = genericDeclareNamedSchema $ S.objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

getQuotes :: (HedisFlow m r, EsqDBFlow m r) => Id SSR.SearchRequest -> m GetQuotesRes
getQuotes searchRequestId = do
  searchRequest <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  offers <- getOffers searchRequest
  estimates <- getEstimates searchRequestId
  return $
    GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity searchRequest.fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity <$> searchRequest.toLocation,
        quotes = offers,
        estimates
      }

getOffers :: (HedisFlow m r, EsqDBFlow m r) => SSR.SearchRequest -> m [OfferRes]
getOffers searchRequest =
  case searchRequest.toLocation of
    Just _ -> do
      quoteList <- QQuote.findAllByRequestId searchRequest.id
      let quotes = OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByNearestDriverDistance quoteList
      metroOffers <- map Metro <$> Metro.getMetroOffers searchRequest.id
      publicTransportOffers <- map PublicTransport <$> PublicTransport.getPublicTransportOffers searchRequest.id
      return . sortBy (compare `on` creationTime) $ quotes <> metroOffers <> publicTransportOffers
    Nothing -> do
      quoteList <- QRentalQuote.findAllByRequestId searchRequest.id
      let quotes = OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByEstimatedFare quoteList
      return . sortBy (compare `on` creationTime) $ quotes
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = compare `on` getMbDistanceToNearestDriver
      sortBy sortFunc quoteList
    getMbDistanceToNearestDriver quote =
      case quote.quoteDetails of
        SQuote.OneWayDetails details -> Just details.distanceToNearestDriver
        SQuote.RentalDetails _ -> Nothing
        SQuote.DriverOfferDetails details -> Just details.distanceToPickup
    creationTime :: OfferRes -> UTCTime
    creationTime (OnDemandCab SQuote.QuoteAPIEntity {createdAt}) = createdAt
    creationTime (Metro Metro.MetroOffer {createdAt}) = createdAt
    creationTime (PublicTransport PublicTransportQuote {createdAt}) = createdAt

getEstimates :: EsqDBFlow m r => Id SSR.SearchRequest -> m [DEstimate.EstimateAPIEntity]
getEstimates searchRequestId = do
  estimateList <- QEstimate.findAllByRequestId searchRequestId
  let estimates = DEstimate.mkEstimateAPIEntity <$> sortByEstimatedFare estimateList
  return . sortBy (compare `on` (.createdAt)) $ estimates

sortByEstimatedFare :: (HasField "estimatedFare" r Money) => [r] -> [r]
sortByEstimatedFare resultList = do
  let sortFunc = compare `on` (.estimatedFare)
  sortBy sortFunc resultList
