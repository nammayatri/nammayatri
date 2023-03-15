{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Quote
  ( GetQuotesRes (..),
    OfferRes (..),
    getQuotes,
  )
where

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
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (objectWithSingleFieldParsing)
import qualified Kernel.Utils.Schema as S
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import qualified SharedLogic.PublicTransport as PublicTransport
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Quote as QRentalQuote
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error

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

getQuotes :: (HedisFlow m r, EsqDBReplicaFlow m r) => Id SSR.SearchRequest -> m GetQuotesRes
getQuotes searchRequestId = do
  searchRequest <- runInReplica $ QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  logDebug $ "search Request is : " <> show searchRequest
  offers <- getOffers searchRequest
  estimates <- getEstimates searchRequestId
  return $
    GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity searchRequest.fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity <$> searchRequest.toLocation,
        quotes = offers,
        estimates
      }

getOffers :: (HedisFlow m r, EsqDBReplicaFlow m r) => SSR.SearchRequest -> m [OfferRes]
getOffers searchRequest = do
  logDebug $ "search Request is : " <> show searchRequest
  case searchRequest.toLocation of
    Just _ -> do
      quoteList <- runInReplica $ QQuote.findAllByRequestId searchRequest.id
      logDebug $ "quotes are : " <> show quoteList
      let quotes = OnDemandCab . SQuote.makeQuoteAPIEntity <$> sortByNearestDriverDistance quoteList
      metroOffers <- map Metro <$> Metro.getMetroOffers searchRequest.id
      publicTransportOffers <- map PublicTransport <$> PublicTransport.getPublicTransportOffers searchRequest.id
      return . sortBy (compare `on` creationTime) $ quotes <> metroOffers <> publicTransportOffers
    Nothing -> do
      quoteList <- runInReplica $ QRentalQuote.findAllByRequestId searchRequest.id
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
        SQuote.OneWaySpecialZoneDetails _ -> Just $ metersToHighPrecMeters $ Meters 0
    creationTime :: OfferRes -> UTCTime
    creationTime (OnDemandCab SQuote.QuoteAPIEntity {createdAt}) = createdAt
    creationTime (Metro Metro.MetroOffer {createdAt}) = createdAt
    creationTime (PublicTransport PublicTransportQuote {createdAt}) = createdAt

getEstimates :: EsqDBReplicaFlow m r => Id SSR.SearchRequest -> m [DEstimate.EstimateAPIEntity]
getEstimates searchRequestId = do
  estimateList <- runInReplica $ QEstimate.findAllByRequestId searchRequestId
  let estimates = DEstimate.mkEstimateAPIEntity <$> sortByEstimatedFare estimateList
  return . sortBy (compare `on` (.createdAt)) $ estimates

sortByEstimatedFare :: (HasField "estimatedFare" r Money) => [r] -> [r]
sortByEstimatedFare resultList = do
  let sortFunc = compare `on` (.estimatedFare)
  sortBy sortFunc resultList
