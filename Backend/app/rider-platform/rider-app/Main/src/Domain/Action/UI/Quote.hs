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
    getQuotesFromInMemory,
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
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Estimate as UEstimate
import qualified Domain.Action.UI.Location as DL
import qualified Domain.Action.UI.MerchantPaymentMethod as DMPM
import qualified Domain.SharedLogic.RideDiscount as RD
import Domain.Types.Booking
import Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.BppDetails (BppDetails)
import Domain.Types.CancellationReason
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.QuoteBreakup as DQB
import qualified Domain.Types.RideStatus as DRide
import Domain.Types.RiderConfig (VehicleServiceTierOrderConfig)
import qualified Domain.Types.RiderConfig as DRC
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
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (objectWithSingleFieldParsing)
import qualified Kernel.Utils.Schema as S
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import qualified SharedLogic.Offer as SOffer
import SharedLogic.Quote
import qualified SharedLogic.Search as SLS
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
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
    whenJust activeBooking $ \booking -> processActiveBooking booking searchRequest.isDashboardRequest OnSearch
  logDebug $ "search Request is : " <> show searchRequest
  let lockKey = estimateBuildLockKey searchRequestId.getId
  Redis.withLockRedisAndReturnValue lockKey 5 $ do
    riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = searchRequest.merchantOperatingCityId.getId})
    quoteList <- QQuote.findAllBySRId searchRequest.id
    estimateList <- QEstimate.findAllBySRId searchRequest.id
    buildGetQuotesRes searchRequest estimateList quoteList riderConfig

-- | Sync-path entry: builds GetQuotesRes from in-memory estimates/quotes
-- produced by 'Domain.Action.Beckn.OnSearch.onSearch'. Skips the Redis
-- estimate-build lock and the duplicate reads of searchRequest, estimates,
-- quotes, and riderConfig that the polling 'getQuotes' performs.
getQuotesFromInMemory ::
  SSR.SearchRequest ->
  [DEstimate.Estimate] ->
  [SQuote.Quote] ->
  Maybe DRC.RiderConfig ->
  Flow GetQuotesRes
getQuotesFromInMemory searchRequest estimateList quoteList mbRiderConfig = do
  activeBooking <- runInReplica $ QBooking.findLatestSelfAndPartyBookingByRiderId searchRequest.riderId
  whenJust activeBooking $ \booking -> processActiveBooking booking searchRequest.isDashboardRequest OnSearch
  buildGetQuotesRes searchRequest estimateList quoteList mbRiderConfig

buildGetQuotesRes ::
  SSR.SearchRequest ->
  [DEstimate.Estimate] ->
  [SQuote.Quote] ->
  Maybe DRC.RiderConfig ->
  Flow GetQuotesRes
buildGetQuotesRes searchRequest estimateList quoteList mbRiderConfig = do
  journeyData <- getJourneys searchRequest searchRequest.hasMultimodalSearch
  person <- QP.findById searchRequest.riderId >>= fromMaybeM (PersonDoesNotExist searchRequest.riderId.getId)
  let mostFrequentVehicleCategory = SLS.mostFrequent person.lastUsedVehicleServiceTiers
      isReferredRide = isJust searchRequest.driverIdentifier
      enableRideHailingOffers = maybe False (.enableRideHailingOffers) mbRiderConfig
  providerLookup <- buildProviderLookup estimateList quoteList
  offers <- getOffers searchRequest enableRideHailingOffers providerLookup quoteList
  estimates' <- getEstimates searchRequest enableRideHailingOffers isReferredRide providerLookup estimateList
  let vehicleServiceTierOrderConfig = maybe [] (.userServiceTierOrderConfig) mbRiderConfig
      defaultServiceTierOrderConfig = maybe [] (.defaultServiceTierOrderConfig) mbRiderConfig
      mbUserConfig = mostFrequentVehicleCategoryConfig mostFrequentVehicleCategory vehicleServiceTierOrderConfig
      estimates = estimatesSorting estimates' mbUserConfig defaultServiceTierOrderConfig
      sortedQuotes = quotesSorting offers mbUserConfig defaultServiceTierOrderConfig
  return $
    GetQuotesRes
      { fromLocation = DL.makeLocationAPIEntity searchRequest.fromLocation,
        toLocation = DL.makeLocationAPIEntity <$> searchRequest.toLocation,
        stops = DL.makeLocationAPIEntity <$> searchRequest.stops,
        quotes = sortedQuotes,
        estimates,
        paymentMethods = [],
        allJourneysLoaded = fromMaybe False searchRequest.allJourneysLoaded,
        journey = journeyData
      }

processActiveBooking :: (CacheFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]) => Booking -> Maybe Bool -> CancellationStage -> m ()
processActiveBooking booking mbIsDashBoardRequest cancellationStage = do
  -- Allow multiple bookings only if request is coming from dashboard
  unless (mbIsDashBoardRequest == Just True || booking.isDashboardRequest == Just True) $ do
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
                      blockOnCancellationRate = Nothing,
                      abortPaytmEdc = Nothing
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

-- | Resolve BppDetails + isValueAddNP per unique providerId across the
-- estimates and quotes lists. One Redis hit per (providerId × source) instead
-- of one per quote/estimate.
buildProviderLookup ::
  [DEstimate.Estimate] ->
  [SQuote.Quote] ->
  Flow (HM.HashMap Text (BppDetails, Bool))
buildProviderLookup estimateList quoteList = do
  let uniqProviderIds = nub $ ((.providerId) <$> estimateList) <> ((.providerId) <$> quoteList)
  entries <- forM uniqProviderIds $ \bppId -> do
    bpp <- CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY)
    v <- CQVAN.isValueAddNP bppId
    pure (bppId, (bpp, v))
  pure $ HM.fromList entries

lookupProvider :: HM.HashMap Text (BppDetails, Bool) -> Text -> Flow (BppDetails, Bool)
lookupProvider providerLookup bppId =
  case HM.lookup bppId providerLookup of
    Just v -> pure v
    Nothing -> throwError $ InternalError $ "BPP details not found for providerId:-" <> bppId

getOffers :: SSR.SearchRequest -> Bool -> HM.HashMap Text (BppDetails, Bool) -> [SQuote.Quote] -> Flow [OfferRes]
getOffers searchRequest enableRideHailingOffers providerLookup quoteList0 = do
  logDebug $ "search Request is : " <> show searchRequest
  let quoteList = case searchRequest.toLocation of
        Just _ -> sortByNearestDriverDistance quoteList0
        Nothing -> sortByEstimatedFare quoteList0
  logDebug $ "quotes are :-" <> show quoteList
  (bppDetailList, isValueAddNPList) <- unzip <$> forM quoteList (\q -> lookupProvider providerLookup q.providerId)
  quoteEntities <- mkQuoteAPIEntitiesWithOffers searchRequest enableRideHailingOffers quoteList bppDetailList isValueAddNPList
  let quotes = case searchRequest.toLocation of
        Just _ ->
          case searchRequest.riderPreferredOption of
            DRPO.Rental -> OnRentalCab <$> quoteEntities
            _ -> OnDemandCab <$> quoteEntities
        Nothing ->
          case searchRequest.isMeterRideSearch of
            Just True -> OnMeterRide <$> quoteEntities
            _ -> OnRentalCab <$> quoteEntities
  return . sortBy (compare `on` offerCreationTime) $ quotes

mkQuoteAPIEntitiesWithOffers ::
  SSR.SearchRequest ->
  Bool ->
  [SQuote.Quote] ->
  [BppDetails] ->
  [Bool] ->
  Flow [QuoteAPIEntity]
mkQuoteAPIEntitiesWithOffers searchReq enableRideHailingOffers quoteList bppDetailList isValueAddNPList = do
  let quoteEntitiesWithCtx =
        zip
          (mkQAPIEntityList quoteList bppDetailList isValueAddNPList)
          ( quoteList <&> \q ->
              let mbBreakup = RD.parseProjectFareParamsBreakup $ quoteBreakupToFareTuple <$> q.quoteBreakupList
                  offerBaseAmount = case mbBreakup of
                    Just b -> b.discountApplicableRideFareTaxExclusive + b.discountApplicableRideFareTax
                    Nothing -> q.estimatedFare.amount
                  offerBasePrice = mkPrice (Just q.estimatedFare.currency) offerBaseAmount
               in (show q.vehicleServiceTierType, mbBreakup, offerBasePrice)
          )
      products = map (\(_, (productId, _, price)) -> (productId, price)) quoteEntitiesWithCtx
  productOffers <-
    if enableRideHailingOffers
      then
        withTryCatch
          "getOffers:offerListWithBasket"
          (SOffer.offerListWithBasket searchReq.merchantId searchReq.riderId searchReq.merchantOperatingCityId DOrder.RideHailing products Nothing Nothing (Just searchReq))
          >>= \case
            Left _ -> pure []
            Right r -> pure r
      else pure []
  let offerMap = Map.fromList productOffers
  forM quoteEntitiesWithCtx $ \(quoteEntity, (productId, mbBreakup, _)) -> do
    mbOffer <- case Map.lookup productId offerMap of
      Nothing -> pure Nothing
      Just resp -> SOffer.mkCumulativeOfferResp searchReq.merchantOperatingCityId resp [] mbBreakup
    pure quoteEntity {customerOffers = mbOffer}

quoteBreakupToFareTuple :: DQB.QuoteBreakup -> (Text, HighPrecMoney)
quoteBreakupToFareTuple qb = (qb.title, qb.price.amount)

sortByNearestDriverDistance :: [SQuote.Quote] -> [SQuote.Quote]
sortByNearestDriverDistance = sortBy (compare `on` getMbDistanceToNearestDriver)
  where
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

offerCreationTime :: OfferRes -> UTCTime
offerCreationTime (OnDemandCab QuoteAPIEntity {createdAt}) = createdAt
offerCreationTime (Metro Metro.MetroOffer {createdAt}) = createdAt
offerCreationTime (OnRentalCab QuoteAPIEntity {createdAt}) = createdAt
offerCreationTime (PublicTransport PublicTransportQuote {createdAt}) = createdAt
offerCreationTime (OnMeterRide QuoteAPIEntity {createdAt}) = createdAt

getEstimates :: SSR.SearchRequest -> Bool -> Bool -> HM.HashMap Text (BppDetails, Bool) -> [DEstimate.Estimate] -> Flow [UEstimate.EstimateAPIEntity]
getEstimates searchRequest enableRideHailingOffers isReferredRide providerLookup estimateList = do
  let sortedEstimates = sortByEstimatedFare estimateList
      estimatesWithCtx =
        map
          ( \e ->
              let mbBreakup = RD.parseProjectFareParamsBreakup $ (\eb -> (eb.title, eb.price.value.amount)) <$> e.estimateBreakupList
                  offerBaseAmount = case mbBreakup of
                    Just b -> b.discountApplicableRideFareTaxExclusive + b.discountApplicableRideFareTax
                    Nothing -> e.estimatedFare.amount
                  offerBasePrice = mkPrice (Just e.estimatedFare.currency) offerBaseAmount
               in (e, mbBreakup, offerBasePrice)
          )
          sortedEstimates
      products = map (\(e, _, price) -> (show e.vehicleServiceTierType, price)) estimatesWithCtx
  productOffers <-
    if enableRideHailingOffers
      then
        withTryCatch
          "getEstimates:offerListWithBasket"
          (SOffer.offerListWithBasket searchRequest.merchantId searchRequest.riderId searchRequest.merchantOperatingCityId DOrder.RideHailing products Nothing Nothing (Just searchRequest))
          >>= \case
            Left _ -> pure []
            Right r -> pure r
      else pure []
  let offerMap = Map.fromList productOffers
  estimates <- forM estimatesWithCtx $ \(estimate, mbBreakup, _) -> do
    let mbOfferResp = Map.lookup (show estimate.vehicleServiceTierType) offerMap
    mbOffer <- case mbOfferResp of
      Nothing -> pure Nothing
      -- Pass the parsed breakup so the offer-response post-offer amount
      -- reflects VAT redistribution via applyRideDiscount.
      Just resp -> SOffer.mkCumulativeOfferResp searchRequest.merchantOperatingCityId resp [] mbBreakup
    (bppDetails, valueAddNP) <- lookupProvider providerLookup estimate.providerId
    UEstimate.mkEstimateAPIEntity isReferredRide mbOffer bppDetails valueAddNP estimate
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

quotesSorting :: [OfferRes] -> Maybe VehicleServiceTierOrderConfig -> [DVST.ServiceTierType] -> [OfferRes]
quotesSorting list mbConfig defaultOrder =
  let order = maybe defaultOrder (.orderArray) mbConfig
   in sortBy (comparing (offerVehicleOrderIndex order)) list
  where
    offerVehicleOrderIndex order = \case
      OnDemandCab q -> vehicleOrderIndex order q.vehicleVariant
      OnRentalCab q -> vehicleOrderIndex order q.vehicleVariant
      OnMeterRide q -> vehicleOrderIndex order q.vehicleVariant
      Metro _ -> maxBound
      PublicTransport _ -> maxBound

vehicleOrderIndex :: [DVST.ServiceTierType] -> DVST.ServiceTierType -> Int
vehicleOrderIndex order v =
  case lookup v (zip order [0 ..]) of
    Just idx -> idx
    Nothing -> maxBound
