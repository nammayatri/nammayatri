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
import EulerHS.Prelude hiding (id)
import Kernel.Randomizer
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (objectWithSingleFieldParsing)
import qualified Kernel.Utils.Schema as S
import SharedLogic.Estimate (EstimateAPIEntity)
import qualified SharedLogic.Estimate as DEstimate
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import qualified SharedLogic.PublicTransport as PublicTransport
import qualified SharedLogic.Types.Booking.Type as DRB
import SharedLogic.Types.Quote (QuoteAPIEntity)
import qualified SharedLogic.Types.Quote as SQuote
import qualified SharedLogic.Types.SearchRequest as SSR
import SharedLogic.Types.SearchRequest.SearchReqLocation (SearchReqLocationAPIEntity)
import qualified SharedLogic.Types.SearchRequest.SearchReqLocation as Location
import qualified SharedLogic.Types.VehicleVariant as DVehicle
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.SimulatedFlow.SearchRequest as CSimulated
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QPerson
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

getQuotes :: (HedisFlow m r, SimluatedCacheFlow m r, EsqDBReplicaFlow m r, MonadFlow m) => Id SSR.SearchRequest -> m GetQuotesRes
getQuotes searchRequestId = do
  mbSearchRequest <- runInReplica $ QSR.findById searchRequestId
  (searchRequest, simulatedFlow) <-
    case mbSearchRequest of
      Just searchReq -> pure (searchReq, False)
      Nothing -> do
        searchReq <- CSimulated.getSearchRequestById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
        person <- runInReplica $ QPerson.findById searchReq.riderId >>= fromMaybeM (PersonNotFound searchReq.riderId.getId)
        pure (searchReq, person.isSimulated)
  if simulatedFlow
    then do
      estimates <- getSimulatedEstimates searchRequest
      pure $
        GetQuotesRes
          { fromLocation = Location.makeSearchReqLocationAPIEntity searchRequest.fromLocation,
            toLocation = Location.makeSearchReqLocationAPIEntity <$> searchRequest.toLocation,
            quotes = [],
            estimates
          }
    else do
      activeBooking <- runInReplica $ QBooking.findLatestByRiderIdAndStatus searchRequest.riderId DRB.activeBookingStatus
      whenJust activeBooking $ \_ -> throwError (InvalidRequest "ACTIVE_BOOKING_ALREADY_PRESENT")
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

getSimulatedEstimates :: (SimluatedCacheFlow m r, MonadFlow m) => HedisFlow m r => SSR.SearchRequest -> m [EstimateAPIEntity]
getSimulatedEstimates searchReq = do
  logDebug $ "simulated search Request is : " <> show searchReq
  now <- getCurrentTime
  guid <- generateGUID
  mRouteInfo <- CSimulated.getRouteInfoBySearchReqId searchReq.id
  distance <-
    case (\routeInfo -> (.getMeters) <$> routeInfo.distance) =<< mRouteInfo of
      Just dis -> pure $ fromIntegral dis
      Nothing -> getRandomInRange (3, 10)
  ratePerKM :: Double <- fromMaybe 15.0 <$> Hedis.get "SimulatedFarePerKM"
  extraFare :: Double <- fromMaybe 10.0 <$> Hedis.get "SimulatedExtraFareToAdd"
  let fare = floor (max 30.0 (distance / 1000.0 * ratePerKM) + extraFare :: Double)
  let estimate =
        DEstimate.EstimateAPIEntity
          { id = guid,
            vehicleVariant = DVehicle.AUTO_RICKSHAW,
            estimatedFare = Money fare,
            estimatedTotalFare = Money fare,
            discount = Nothing,
            totalFareRange = DEstimate.FareRange (Money fare) (Money $ fare + 20),
            agencyName = "",
            agencyNumber = "",
            agencyCompletedRidesCount = 0,
            tripTerms = [],
            createdAt = now,
            estimateFareBreakup = [],
            nightShiftRate = Nothing,
            nightShiftInfo = Nothing,
            waitingCharges = DEstimate.WaitingCharges Nothing,
            driversLatLong = []
          }
  CSimulated.cacheByEstimateId guid estimate searchReq
  return [estimate]

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
  estimateList <- runInReplica $ QEstimate.findAllBySRId searchRequestId
  let estimates = DEstimate.mkEstimateAPIEntity <$> sortByEstimatedFare estimateList
  return . sortBy (compare `on` (.createdAt)) $ estimates

sortByEstimatedFare :: (HasField "estimatedFare" r Money) => [r] -> [r]
sortByEstimatedFare resultList = do
  let sortFunc = compare `on` (.estimatedFare)
  sortBy sortFunc resultList
