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
    estimateBuildLockKey,
    processActiveBooking,
    mkQAPIEntityList,
    mkQuoteBreakupAPIEntity,
    QuoteAPIEntity (..),
    QuoteBreakupAPIEntity (..),
    JourneyData (..),
    JourneyLeg (..),
    getJourneys,
  )
where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified BecknV2.FRFS.Enums as FRFSEnums
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Estimate as UEstimate
import qualified Domain.Action.UI.Location as DL
import qualified Domain.Action.UI.MerchantPaymentMethod as DMPM
import Domain.Types.Booking
import Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.CancellationReason
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.RideStatus as DRide
import Domain.Types.RiderConfig (VehicleServiceTierOrderConfig)
import qualified Domain.Types.RiderPreferredOption as DRPO
import Domain.Types.RouteDetails
import qualified Domain.Types.SearchRequest as SSR
import Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.Trip as DTrip
import Environment
import EulerHS.Prelude hiding (find, group, id, length, map, maximumBy, sum)
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import Kernel.Prelude hiding (whenJust)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (objectWithSingleFieldParsing)
import qualified Kernel.Utils.Schema as S
import qualified Lib.JourneyModule.Base as JM
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import SharedLogic.Quote
import qualified SharedLogic.Search as SLS
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import TransactionLogs.Types

data GetQuotesRes = GetQuotesRes
  { fromLocation :: DL.LocationAPIEntity,
    toLocation :: Maybe DL.LocationAPIEntity,
    stops :: [DL.LocationAPIEntity],
    quotes :: [OfferRes],
    estimates :: [UEstimate.EstimateAPIEntity],
    paymentMethods :: [DMPM.PaymentMethodAPIEntity],
    allJourneysLoaded :: Bool,
    journey :: Maybe [JourneyData]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data JourneyData = JourneyData
  { totalMinFare :: HighPrecMoney,
    totalMaxFare :: HighPrecMoney,
    duration :: Maybe Seconds,
    distance :: Distance,
    modes :: [DTrip.MultimodalTravelMode],
    startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    journeyId :: Id DJ.Journey,
    journeyLegs :: [JourneyLeg],
    relevanceScore :: Double,
    hasPreferredServiceTier :: Maybe Bool,
    hasPreferredTransitModes :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data JourneyLeg = JourneyLeg
  { journeyLegOrder :: Int,
    journeyMode :: DTrip.MultimodalTravelMode,
    journeyLegId :: Id DJL.JourneyLeg,
    fromLatLong :: LatLong,
    toLatLong :: LatLong,
    fromStationCode :: Maybe Text,
    toStationCode :: Maybe Text,
    routeDetails :: [RouteDetail],
    color :: Maybe Text, -- TODO :: Deprecated, Moved to RouteDetail
    colorCode :: Maybe Text, -- TODO :: Deprecated, Moved to RouteDetail
    duration :: Maybe Seconds,
    distance :: Maybe Distance,
    liveVehicleAvailableServiceTypes :: Maybe [FRFSEnums.ServiceTierType],
    estimatedMinFare :: Maybe HighPrecMoney,
    estimatedMaxFare :: Maybe HighPrecMoney,
    validTill :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RouteDetail = RouteDetail
  { routeCode :: Maybe Text,
    fromStationCode :: Maybe Text,
    toStationCode :: Maybe Text,
    alternateShortNames :: [Text],
    alternateRouteIds :: Maybe [Text],
    color :: Maybe Text,
    colorCode :: Maybe Text,
    fromStationLatLong :: LatLong,
    toStationLatLong :: LatLong,
    fromStationPlatformCode :: Maybe Text,
    toStationPlatformCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- TODO: Needs to be fixed as quotes could be of both rentals and one way
data OfferRes
  = OnDemandCab QuoteAPIEntity
  | OnRentalCab QuoteAPIEntity
  | Metro MetroOffer
  | PublicTransport PublicTransportQuote
  | OnMeterRide QuoteAPIEntity
  deriving (Show, Generic)

instance ToJSON OfferRes where
  toJSON = genericToJSON $ objectWithSingleFieldParsing safeToLower

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing safeToLower

instance ToSchema OfferRes where
  declareNamedSchema = genericDeclareNamedSchema $ S.objectWithSingleFieldParsing safeToLower

safeToLower :: String -> String
safeToLower (f : rest) = toLower f : rest
safeToLower [] = []

estimateBuildLockKey :: Text -> Text
estimateBuildLockKey searchReqid = "Customer:Estimate:Build:" <> searchReqid

getQuotes :: Id SSR.SearchRequest -> Maybe Bool -> Flow GetQuotesRes
getQuotes searchRequestId mbAllowMultiple = do
  searchRequest <- runInReplica $ QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  unless (mbAllowMultiple == Just True) $ do
    activeBooking <- runInReplica $ QBooking.findLatestSelfAndPartyBookingByRiderId searchRequest.riderId
    whenJust activeBooking $ \booking -> processActiveBooking booking (Just searchRequest.riderPreferredOption) OnSearch
  logDebug $ "search Request is : " <> show searchRequest
  journeyData <- getJourneys searchRequest searchRequest.hasMultimodalSearch
  person <- QP.findById searchRequest.riderId >>= fromMaybeM (PersonDoesNotExist searchRequest.riderId.getId)
  let mostFrequentVehicleCategory = SLS.mostFrequent person.lastUsedVehicleServiceTiers
  let lockKey = estimateBuildLockKey searchRequestId.getId
  Redis.withLockRedisAndReturnValue lockKey 5 $ do
    offers <- getOffers searchRequest
    estimates' <- getEstimates searchRequestId (isJust searchRequest.driverIdentifier) -- TODO(MultiModal): only check for estimates which are done
    riderConfig <- QRC.findByMerchantOperatingCityId (cast searchRequest.merchantOperatingCityId) Nothing

    let vehicleServiceTierOrderConfig = maybe [] (.userServiceTierOrderConfig) riderConfig
        defaultServiceTierOrderConfig = maybe [] (.defaultServiceTierOrderConfig) riderConfig
        estimates = estimatesSorting estimates' (mostFrequentVehicleCategoryConfig mostFrequentVehicleCategory vehicleServiceTierOrderConfig) defaultServiceTierOrderConfig
    return $
      GetQuotesRes
        { fromLocation = DL.makeLocationAPIEntity searchRequest.fromLocation,
          toLocation = DL.makeLocationAPIEntity <$> searchRequest.toLocation,
          stops = DL.makeLocationAPIEntity <$> searchRequest.stops,
          quotes = offers,
          estimates,
          paymentMethods = [],
          allJourneysLoaded = fromMaybe False searchRequest.allJourneysLoaded,
          journey = journeyData
        }

processActiveBooking :: (CacheFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]) => Booking -> Maybe DRPO.RiderPreferredOption -> CancellationStage -> m ()
processActiveBooking booking mbNewRiderPreferredOption cancellationStage = do
  -- Check if both bookings are FixedRoute and allow multiple bookings
  -- Fetch the active booking's SearchRequest through its Quote
  mbActiveRiderPreferredOption <- case booking.quoteId of
    Just quoteId -> do
      mbQuote <- runInReplica $ QQuote.findById quoteId
      case mbQuote of
        Just quote -> do
          mbActiveSearchRequest <- runInReplica $ QSR.findById quote.requestId
          pure $ (.riderPreferredOption) <$> mbActiveSearchRequest
        Nothing -> pure Nothing
    Nothing -> pure Nothing
  let isFixedRouteMultipleAllowed = canCoexistForFixedRoute mbNewRiderPreferredOption mbActiveRiderPreferredOption

  -- Allow multiple bookings only if both are FixedRoute (based on riderPreferredOption)
  unless isFixedRouteMultipleAllowed $ do
    mbRide <- QRide.findActiveByRBId booking.id
    case mbRide of
      Just ride -> do
        unless (ride.status == DRide.UPCOMING) $ throwError (InvalidRequest "ACTIVE_BOOKING_ALREADY_PRESENT")
      Nothing -> do
        now <- getCurrentTime
        if addUTCTime 900 booking.startTime < now || not (isRentalOrInterCity booking.bookingDetails) || (addUTCTime 120 booking.startTime < now && isHighPriorityBooking booking.bookingDetails)
          then do
            let cancelReq =
                  DCancel.CancelReq
                    { reasonCode = CancellationReasonCode "Active booking",
                      reasonStage = cancellationStage,
                      additionalInfo = Nothing,
                      reallocate = Nothing,
                      blockOnCancellationRate = Nothing
                    }
            fork "active booking processing" $ do
              dCancelRes <- DCancel.cancel booking Nothing cancelReq SBCR.ByApplication
              void . withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes Nothing
          else throwError (InvalidRequest "ACTIVE_BOOKING_ALREADY_PRESENT")

isRentalOrInterCity :: DBooking.BookingDetails -> Bool
isRentalOrInterCity bookingDetails = case bookingDetails of
  DBooking.RentalDetails _ -> True
  DBooking.InterCityDetails _ -> True
  _ -> False

isHighPriorityBooking :: DBooking.BookingDetails -> Bool
isHighPriorityBooking bookingDetails = case bookingDetails of
  DBooking.AmbulanceDetails _ -> True
  _ -> False

-- | Check if two bookings can coexist based on FixedRoute riderPreferredOption
-- Allows multiple bookings only if both bookings have riderPreferredOption = FixedRoute
canCoexistForFixedRoute :: Maybe DRPO.RiderPreferredOption -> Maybe DRPO.RiderPreferredOption -> Bool
canCoexistForFixedRoute mbNewRiderPreferredOption mbActiveRiderPreferredOption =
  case (mbNewRiderPreferredOption, mbActiveRiderPreferredOption) of
    (Just DRPO.FixedRoute, Just DRPO.FixedRoute) -> True
    _ -> False

getOffers :: (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => SSR.SearchRequest -> m [OfferRes]
getOffers searchRequest = do
  logDebug $ "search Request is : " <> show searchRequest
  case searchRequest.toLocation of
    Just _ -> do
      quoteList <- sortByNearestDriverDistance <$> runInReplica (QQuote.findAllBySRId searchRequest.id)
      logDebug $ "quotes are :-" <> show quoteList
      bppDetailList <- forM ((.providerId) <$> quoteList) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.subscriberId
      let quotes = case searchRequest.riderPreferredOption of
            DRPO.Rental -> OnRentalCab <$> mkQAPIEntityList quoteList bppDetailList isValueAddNPList
            _ -> OnDemandCab <$> mkQAPIEntityList quoteList bppDetailList isValueAddNPList
      return . sortBy (compare `on` creationTime) $ quotes
    Nothing -> do
      quoteList <- sortByEstimatedFare <$> runInReplica (QQuote.findAllBySRId searchRequest.id)
      logDebug $ "quotes are :-" <> show quoteList
      bppDetailList <- forM ((.providerId) <$> quoteList) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.subscriberId
      let quotes = case searchRequest.isMeterRideSearch of
            Just True -> OnMeterRide <$> mkQAPIEntityList quoteList bppDetailList isValueAddNPList
            _ -> OnRentalCab <$> mkQAPIEntityList quoteList bppDetailList isValueAddNPList
      return . sortBy (compare `on` creationTime) $ quotes
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = compare `on` getMbDistanceToNearestDriver
      sortBy sortFunc quoteList
    getMbDistanceToNearestDriver quote =
      case quote.quoteDetails of
        SQuote.MeterRideDetails _ -> Nothing
        SQuote.OneWayDetails details -> Just details.distanceToNearestDriver
        SQuote.AmbulanceDetails details -> details.distanceToPickup
        SQuote.DeliveryDetails details -> details.distanceToPickup
        SQuote.RentalDetails _ -> Nothing
        SQuote.DriverOfferDetails details -> details.distanceToPickup
        SQuote.OneWaySpecialZoneDetails _ -> Just $ Distance 0 Meter
        SQuote.InterCityDetails _ -> Just $ Distance 0 Meter
    creationTime :: OfferRes -> UTCTime
    creationTime (OnDemandCab QuoteAPIEntity {createdAt}) = createdAt
    creationTime (Metro Metro.MetroOffer {createdAt}) = createdAt
    creationTime (OnRentalCab QuoteAPIEntity {createdAt}) = createdAt
    creationTime (PublicTransport PublicTransportQuote {createdAt}) = createdAt
    creationTime (OnMeterRide QuoteAPIEntity {createdAt}) = createdAt

getEstimates :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SSR.SearchRequest -> Bool -> m [UEstimate.EstimateAPIEntity]
getEstimates searchRequestId isReferredRide = do
  estimateList <- runInReplica $ QEstimate.findAllBySRId searchRequestId
  estimates <- mapM (UEstimate.mkEstimateAPIEntity isReferredRide) (sortByEstimatedFare estimateList)
  return . sortBy (compare `on` (.createdAt)) $ estimates

sortByEstimatedFare :: (HasField "estimatedFare" r Price) => [r] -> [r]
sortByEstimatedFare resultList = do
  let sortFunc = compare `on` (.estimatedFare.amount)
  sortBy sortFunc resultList

getJourneys :: SSR.SearchRequest -> Maybe Bool -> Flow (Maybe [JourneyData])
getJourneys searchRequest hasMultimodalSearch = do
  case hasMultimodalSearch of
    Just True -> do
      allJourneys :: [DJ.Journey] <- QJourney.findBySearchId searchRequest.id.getId
      journeyData <-
        forM allJourneys \journey -> do
          legs <- QJourneyLeg.getJourneyLegs journey.id
          legsInfo <- JM.getAllLegsInfoWithoutSearch searchRequest.riderId journey.id
          journeyLegs <- do
            forM legs \journeyLeg -> do
              let legInfo = find (\leg -> Just leg.searchId == journeyLeg.legSearchId) legsInfo
              return $
                JourneyLeg
                  { journeyLegOrder = journeyLeg.sequenceNumber,
                    journeyMode = journeyLeg.mode,
                    journeyLegId = journeyLeg.id,
                    fromLatLong = LatLong {lat = journeyLeg.startLocation.latitude, lon = journeyLeg.startLocation.longitude},
                    toLatLong = LatLong {lat = journeyLeg.endLocation.latitude, lon = journeyLeg.endLocation.longitude},
                    fromStationCode = journeyLeg.fromStopDetails >>= (.stopCode),
                    toStationCode = journeyLeg.toStopDetails >>= (.stopCode),
                    color = listToMaybe $ catMaybes $ map (.routeShortName) journeyLeg.routeDetails,
                    colorCode = listToMaybe $ catMaybes $ map (.routeColorCode) journeyLeg.routeDetails,
                    routeDetails = map mkRouteDetail journeyLeg.routeDetails,
                    duration = journeyLeg.duration,
                    liveVehicleAvailableServiceTypes = journeyLeg.liveVehicleAvailableServiceTypes,
                    distance = journeyLeg.distance,
                    estimatedMinFare = (legInfo >>= (.estimatedMinFare) <&> (.amount)) <|> journeyLeg.estimatedMinFare,
                    estimatedMaxFare = (legInfo >>= (.estimatedMaxFare) <&> (.amount)) <|> journeyLeg.estimatedMaxFare,
                    validTill = legInfo >>= (.validTill)
                  }
          let estimatedMinFare = sum $ mapMaybe (.estimatedMinFare) journeyLegs
          let estimatedMaxFare = sum $ mapMaybe (.estimatedMaxFare) journeyLegs
          return $
            JourneyData
              { totalMinFare = estimatedMinFare,
                totalMaxFare = estimatedMaxFare,
                modes = journey.modes,
                journeyLegs = sortOn (.journeyLegOrder) journeyLegs,
                startTime = journey.startTime,
                endTime = journey.endTime,
                journeyId = journey.id,
                duration = journey.estimatedDuration,
                distance = journey.estimatedDistance,
                relevanceScore = fromMaybe 1 journey.relevanceScore, -- 1 is the max possible score.
                hasPreferredServiceTier = journey.hasPreferredServiceTier,
                hasPreferredTransitModes = journey.hasPreferredTransitModes
              }
      return . Just $ sortOn (.relevanceScore) journeyData
    _ -> return Nothing
  where
    mkRouteDetail :: RouteDetails -> RouteDetail
    mkRouteDetail routeDetail =
      RouteDetail
        { routeCode = gtfsIdtoDomainCode <$> routeDetail.routeGtfsId,
          fromStationCode = (gtfsIdtoDomainCode <$> (routeDetail.fromStopCode)) <|> (gtfsIdtoDomainCode <$> routeDetail.fromStopGtfsId),
          toStationCode = (gtfsIdtoDomainCode <$> (routeDetail.toStopCode)) <|> (gtfsIdtoDomainCode <$> routeDetail.toStopGtfsId),
          color = routeDetail.routeShortName,
          colorCode = routeDetail.routeShortName,
          alternateShortNames = routeDetail.alternateShortNames,
          alternateRouteIds = routeDetail.alternateRouteIds,
          fromStationLatLong =
            LatLong
              { lat = routeDetail.startLocationLat,
                lon = routeDetail.startLocationLon
              },
          toStationLatLong =
            LatLong
              { lat = routeDetail.endLocationLat,
                lon = routeDetail.endLocationLon
              },
          fromStationPlatformCode = routeDetail.fromStopPlatformCode,
          toStationPlatformCode = routeDetail.toStopPlatformCode
        }

mostFrequentVehicleCategoryConfig :: Maybe DVST.ServiceTierType -> [VehicleServiceTierOrderConfig] -> Maybe VehicleServiceTierOrderConfig
mostFrequentVehicleCategoryConfig Nothing _ = Nothing
mostFrequentVehicleCategoryConfig (Just vehicleServiceTier) orderArray =
  find (\v -> v.vehicle == vehicleServiceTier) orderArray

-- Sorting function
estimatesSorting :: [UEstimate.EstimateAPIEntity] -> Maybe VehicleServiceTierOrderConfig -> [DVST.ServiceTierType] -> [UEstimate.EstimateAPIEntity]
estimatesSorting list Nothing order = sortBy (comparing (\estimate -> vehicleOrderIndex order estimate.serviceTierType)) list
estimatesSorting list (Just config) _ =
  sortBy (comparing (\estimate -> vehicleOrderIndex config.orderArray estimate.serviceTierType)) list

vehicleOrderIndex :: [DVST.ServiceTierType] -> DVST.ServiceTierType -> Int
vehicleOrderIndex order v =
  case lookup v (zip order [0 ..]) of
    Just idx -> idx
    Nothing -> maxBound
