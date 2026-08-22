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
    SuggestedEstimates (..),
    SuggestedOption (..),
    AlternateSuggestion (..),
    AlternateSuggestionsRes (..),
    mkSuggestedEstimates,
    mkSuggestedOption,
    loadAlternateSuggestions,
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
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Location as DL
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.QuoteBreakup as DQB
import qualified Domain.Types.RideStatus as DRide
import Domain.Types.RiderConfig (VehicleServiceTierOrderConfig)
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.RiderPreferredOption as DRPO
import Domain.Types.RouteDetailsAPI (RouteDetail, mkRouteDetail)
import qualified Domain.Types.SearchRequest as SSR
import Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.Trip as DTrip
import Environment
import EulerHS.Prelude hiding (find, group, id, length, map, maximumBy, null, sum)
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import qualified Kernel.External.Types as Lang
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
import qualified SharedLogic.BetterRoutePoint as BRP
import qualified SharedLogic.BetterRoutePointCache as BRPC
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import qualified SharedLogic.Offer as SOffer
import SharedLogic.Quote
import qualified SharedLogic.Search as SLS
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Translations as CQTranslations
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
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
    journey :: Maybe [JourneyData],
    suggestedEstimates :: Maybe SuggestedEstimates
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- | Estimates for a nearby pickup/drop that cuts a detour out of the ride, offered
-- alongside the estimates for what the customer actually asked for. Their own pickup and
-- drop are never overridden -- picking one of these is an explicit choice, and the
-- estimate ids here belong to a separate search request that select/init resolve on their
-- own.
data SuggestedEstimates = SuggestedEstimates
  { -- | The search request these estimates belong to. Selecting any of them books this one.
    searchId :: Id SSR.SearchRequest,
    estimates :: [UEstimate.EstimateAPIEntity],
    -- | Where to walk from, and to. Absent when that end of the ride is unchanged.
    suggestedPickup :: Maybe DL.LocationAPIEntity,
    suggestedDrop :: Maybe DL.LocationAPIEntity,
    walkDistanceToPickup :: Maybe Meters,
    walkDistanceFromDrop :: Maybe Meters,
    -- | How much shorter the ride becomes.
    rideDistanceSaved :: Meters,
    -- | The other ways this ride could be reshaped, unpriced. Fares for these are only
    -- fetched when the customer asks for one, via /rideSearch/suggestedFare.
    alternatives :: [SuggestedOption]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- | A walk-and-save shape other than the default, described well enough to draw on a map
-- the moment the search answers. Its fare is not here: pricing was dispatched in the
-- background, and lands via /alternateSuggestion/{searchId}/result -- match it up by
-- 'searchId'. Points are bare coordinates because the app names them with its own
-- geocoder, and sends the name back when the customer selects one.
data SuggestedOption = SuggestedOption
  { -- | The shadow search being priced for this shape.
    searchId :: Id SSR.SearchRequest,
    kind :: BRP.BetterPointKind,
    suggestedPickup :: Maybe LatLong,
    suggestedDrop :: Maybe LatLong,
    walkDistanceToPickup :: Maybe Meters,
    walkDistanceFromDrop :: Maybe Meters,
    rideDistanceSaved :: Meters,
    estimatedRideDistance :: Meters,
    estimatedRideDuration :: Maybe Seconds
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkSuggestedOption :: BRPC.AlternateShadow -> SuggestedOption
mkSuggestedOption alternate =
  let betterRoute = alternate.route
   in SuggestedOption
        { searchId = alternate.searchId,
          kind = betterRoute.kind,
          suggestedPickup = (.point) <$> betterRoute.betterPickup,
          suggestedDrop = (.point) <$> betterRoute.betterDrop,
          walkDistanceToPickup = (.walkDistance) <$> betterRoute.betterPickup,
          walkDistanceFromDrop = (.walkDistance) <$> betterRoute.betterDrop,
          rideDistanceSaved = betterRoute.totalRideDistanceSaved,
          estimatedRideDistance = betterRoute.newRouteDistance,
          estimatedRideDuration = betterRoute.newRouteDuration
        }

-- | One alternate's fares, once the background dispatch has been answered.
data AlternateSuggestion = AlternateSuggestion
  { searchId :: Id SSR.SearchRequest,
    kind :: BRP.BetterPointKind,
    estimates :: [UEstimate.EstimateAPIEntity],
    suggestedPickup :: Maybe DL.LocationAPIEntity,
    suggestedDrop :: Maybe DL.LocationAPIEntity,
    walkDistanceToPickup :: Maybe Meters,
    walkDistanceFromDrop :: Maybe Meters,
    rideDistanceSaved :: Meters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- | Alternates are priced in the background, so this is a poll: 'allLoaded' is False while
-- at least one is still outstanding, and the list grows across calls.
data AlternateSuggestionsRes = AlternateSuggestionsRes
  { alternates :: [AlternateSuggestion],
    allLoaded :: Bool
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

translateServiceTierText :: Id DMOC.MerchantOperatingCity -> Lang.Language -> Maybe Text -> Flow (Maybe Text)
translateServiceTierText mocId language mbText = case mbText of
  Just text ->
    getConfig
      (TranslationDimensions {merchantOperatingCityId = Just mocId.getId, messageKey = text, language = Just language})
      (Just (CQTranslations.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache mocId text language))
      <&> Just . maybe text (.message)
  Nothing -> pure Nothing

getQuotes :: Id SSR.SearchRequest -> Maybe Bool -> Flow GetQuotesRes
getQuotes searchRequestId mbAllowMultiple = do
  searchRequest <- runInReplica $ QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  unless (mbAllowMultiple == Just True) $ do
    activeBooking <- runInReplica $ QBooking.findLatestSelfAndPartyBookingByRiderId searchRequest.riderId
    whenJust activeBooking $ \booking -> processActiveBooking booking searchRequest.isDashboardRequest OnSearch
  logDebug $ "search Request is : " <> show searchRequest
  let lockKey = estimateBuildLockKey searchRequestId.getId
  Redis.withLockRedisAndReturnValue lockKey 5 $ do
    riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = searchRequest.merchantOperatingCityId.getId}) Nothing
    quoteList <- QQuote.findAllBySRId searchRequest.id
    estimateList <- QEstimate.findAllBySRId searchRequest.id
    res <- buildGetQuotesRes searchRequest estimateList quoteList riderConfig
    -- The sync path already has the suggestion in hand and overrides this field; on the
    -- polling path the shadow search has been persisting its estimates in the background
    -- since /rideSearch, so read them here.
    mbSuggested <- loadSuggestedEstimates searchRequest
    pure res {suggestedEstimates = mbSuggested}

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
      language = fromMaybe Lang.ENGLISH person.language
  providerLookup <- buildProviderLookup estimateList quoteList
  offers <- getOffers searchRequest enableRideHailingOffers providerLookup quoteList language
  estimates' <- getEstimates searchRequest enableRideHailingOffers isReferredRide providerLookup estimateList language
  let vehicleServiceTierOrderConfig = maybe [] (.userServiceTierOrderConfig) mbRiderConfig
      defaultServiceTierOrderConfig = maybe [] (.defaultServiceTierOrderConfig) mbRiderConfig
      specialLocationTierOrderConfig = getSpecialLocationTierOrder searchRequest.discoveredSpecialLocationId mbRiderConfig
      mbUserConfig = if null specialLocationTierOrderConfig then mostFrequentVehicleCategoryConfig mostFrequentVehicleCategory vehicleServiceTierOrderConfig else specialLocationTierOrderConfig
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
        journey = journeyData,
        -- Filled in by the caller: the sync path passes it inline, the polling path reads
        -- it from the shadow search. See loadSuggestedEstimates.
        suggestedEstimates = Nothing
      }

-- | The better-route-point suggestion for a search, if a shadow search was created for it
-- and the BPP has answered. Absent is the normal case.
loadSuggestedEstimates :: SSR.SearchRequest -> Flow (Maybe SuggestedEstimates)
loadSuggestedEstimates searchRequest
  -- A shadow never has a shadow of its own; asking would be a pointless read.
  | isJust searchRequest.parentSearchRequestId = pure Nothing
  | otherwise =
    QSR.findFirstByParentSearchRequestId searchRequest.id >>= \case
      Nothing -> pure Nothing
      Just shadow -> do
        shadowEstimates <- QEstimate.findAllBySRId shadow.id
        -- Geometry only: the alternates' fares are still being fetched in the background,
        -- and are collected separately through 'loadAlternateSuggestions'. Absent once the
        -- search's cached context has expired.
        alternates <- maybe [] (.alternates) <$> BRPC.getSuggestedSearchCtx searchRequest.id
        mkSuggestedEstimates shadow shadowEstimates (mkSuggestedOption <$> alternates)

-- | Renders a shadow search's estimates for the customer. Takes the shadow search request
-- itself, since that is what carries the moved pickup/drop and the saving.
mkSuggestedEstimates :: SSR.SearchRequest -> [DEstimate.Estimate] -> [SuggestedOption] -> Flow (Maybe SuggestedEstimates)
mkSuggestedEstimates shadow estimateList alternatives = do
  -- Nothing to suggest without both a priced estimate and a saving to justify the walk.
  case (nonEmpty estimateList, shadow.betterPointRideDistanceSaved) of
    (Just _, Just rideDistanceSaved) -> do
      person <- QP.findById shadow.riderId >>= fromMaybeM (PersonDoesNotExist shadow.riderId.getId)
      riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = shadow.merchantOperatingCityId.getId}) Nothing
      let enableRideHailingOffers = maybe False (.enableRideHailingOffers) riderConfig
          isReferredRide = isJust shadow.driverIdentifier
          language = fromMaybe Lang.ENGLISH person.language
      providerLookup <- buildProviderLookup estimateList []
      apiEstimates <- getEstimates shadow enableRideHailingOffers isReferredRide providerLookup estimateList language
      pure . Just $
        SuggestedEstimates
          { searchId = shadow.id,
            estimates = apiEstimates,
            -- Only the end that actually moved gets a location; the other is unchanged from
            -- what the customer entered, so repeating it would imply a walk that isn't there.
            suggestedPickup = DL.makeLocationAPIEntity shadow.fromLocation <$ shadow.betterPointWalkToPickup,
            suggestedDrop = shadow.betterPointWalkFromDrop >> (DL.makeLocationAPIEntity <$> shadow.toLocation),
            walkDistanceToPickup = shadow.betterPointWalkToPickup,
            walkDistanceFromDrop = shadow.betterPointWalkFromDrop,
            rideDistanceSaved,
            alternatives
          }
    _ -> pure Nothing

-- | The fares for the alternate shapes, as far as they have arrived.
--
-- Their shadow searches were created and dispatched during /rideSearch and never waited
-- on, so this is a poll: an alternate the provider has not answered for yet is simply
-- absent, and 'allLoaded' stays False until every one of them is in.
loadAlternateSuggestions :: SSR.SearchRequest -> Flow AlternateSuggestionsRes
loadAlternateSuggestions parent = do
  BRPC.getSuggestedSearchCtx parent.id >>= \case
    -- No context means no suggestion was ever found for this search, or it has expired.
    -- Either way there is nothing still coming, so this is loaded, not pending.
    Nothing -> pure AlternateSuggestionsRes {alternates = [], allLoaded = True}
    Just ctx -> do
      resolved <- forM ctx.alternates $ \alternate -> runMaybeT $ do
        shadow <- MaybeT $ QSR.findById alternate.searchId
        estimateList <- lift $ QEstimate.findAllBySRId shadow.id
        MaybeT $ mkAlternateSuggestion shadow alternate.route estimateList
      let loaded = catMaybes resolved
      pure
        AlternateSuggestionsRes
          { alternates = loaded,
            allLoaded = length loaded == length ctx.alternates
          }

mkAlternateSuggestion :: SSR.SearchRequest -> BRP.BetterRoute -> [DEstimate.Estimate] -> Flow (Maybe AlternateSuggestion)
mkAlternateSuggestion shadow route estimateList =
  case (nonEmpty estimateList, shadow.betterPointRideDistanceSaved) of
    (Just _, Just rideDistanceSaved) -> do
      person <- QP.findById shadow.riderId >>= fromMaybeM (PersonDoesNotExist shadow.riderId.getId)
      riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = shadow.merchantOperatingCityId.getId}) Nothing
      let enableRideHailingOffers = maybe False (.enableRideHailingOffers) riderConfig
          isReferredRide = isJust shadow.driverIdentifier
          language = fromMaybe Lang.ENGLISH person.language
      providerLookup <- buildProviderLookup estimateList []
      apiEstimates <- getEstimates shadow enableRideHailingOffers isReferredRide providerLookup estimateList language
      pure . Just $
        AlternateSuggestion
          { searchId = shadow.id,
            kind = route.kind,
            estimates = apiEstimates,
            suggestedPickup = DL.makeLocationAPIEntity shadow.fromLocation <$ shadow.betterPointWalkToPickup,
            suggestedDrop = shadow.betterPointWalkFromDrop >> (DL.makeLocationAPIEntity <$> shadow.toLocation),
            walkDistanceToPickup = shadow.betterPointWalkToPickup,
            walkDistanceFromDrop = shadow.betterPointWalkFromDrop,
            rideDistanceSaved
          }
    _ -> pure Nothing

processActiveBooking :: (CacheFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig], HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl]) => Booking -> Maybe Bool -> CancellationStage -> m ()
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

getOffers :: SSR.SearchRequest -> Bool -> HM.HashMap Text (BppDetails, Bool) -> [SQuote.Quote] -> Lang.Language -> Flow [OfferRes]
getOffers searchRequest enableRideHailingOffers providerLookup quoteList0 language = do
  logDebug $ "search Request is : " <> show searchRequest
  let quoteList = case searchRequest.toLocation of
        Just _ -> sortByNearestDriverDistance quoteList0
        Nothing -> sortByEstimatedFare quoteList0
  logDebug $ "quotes are :-" <> show quoteList
  (bppDetailList, isValueAddNPList) <- unzip <$> forM quoteList (\q -> lookupProvider providerLookup q.providerId)
  quoteEntities <- mkQuoteAPIEntitiesWithOffers searchRequest enableRideHailingOffers quoteList bppDetailList isValueAddNPList language
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
  Lang.Language ->
  Flow [QuoteAPIEntity]
mkQuoteAPIEntitiesWithOffers searchReq enableRideHailingOffers quoteList bppDetailList isValueAddNPList language = do
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
      Just resp -> SOffer.mkCumulativeOfferResp searchReq.merchantOperatingCityId resp [] mbBreakup Nothing
    serviceTierName <- translateServiceTierText searchReq.merchantOperatingCityId language quoteEntity.serviceTierName
    serviceTierShortDesc <- translateServiceTierText searchReq.merchantOperatingCityId language quoteEntity.serviceTierShortDesc
    pure quoteEntity {customerOffers = mbOffer, SharedLogic.Quote.serviceTierName = serviceTierName, SharedLogic.Quote.serviceTierShortDesc = serviceTierShortDesc}

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
        -- No nearest-driver-distance concept tracked for EasyBooking quotes, same as Rental.
        SQuote.EasyBookingDetails _ -> Nothing
        SQuote.DriverOfferDetails details -> details.distanceToPickup
        SQuote.OneWaySpecialZoneDetails _ -> Just $ Distance 0 Meter
        SQuote.InterCityDetails _ -> Just $ Distance 0 Meter

offerCreationTime :: OfferRes -> UTCTime
offerCreationTime (OnDemandCab QuoteAPIEntity {createdAt}) = createdAt
offerCreationTime (Metro Metro.MetroOffer {createdAt}) = createdAt
offerCreationTime (OnRentalCab QuoteAPIEntity {createdAt}) = createdAt
offerCreationTime (PublicTransport PublicTransportQuote {createdAt}) = createdAt
offerCreationTime (OnMeterRide QuoteAPIEntity {createdAt}) = createdAt

getEstimates :: SSR.SearchRequest -> Bool -> Bool -> HM.HashMap Text (BppDetails, Bool) -> [DEstimate.Estimate] -> Lang.Language -> Flow [UEstimate.EstimateAPIEntity]
getEstimates searchRequest _enableRideHailingOffers isReferredRide providerLookup estimateList language = do
  let sortedEstimates = sortByEstimatedFare estimateList
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = searchRequest.merchantOperatingCityId.getId}) Nothing
  let enableRideHailingOffers = maybe False (.enableRideHailingOffers) riderConfig
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
      Just resp -> SOffer.mkCumulativeOfferResp searchRequest.merchantOperatingCityId resp [] mbBreakup Nothing
    (bppDetails, valueAddNP) <- lookupProvider providerLookup estimate.providerId
    apiEntity <- UEstimate.mkEstimateAPIEntity isReferredRide mbOffer bppDetails valueAddNP estimate
    serviceTierName <- translateServiceTierText searchRequest.merchantOperatingCityId language apiEntity.serviceTierName
    serviceTierShortDesc <- translateServiceTierText searchRequest.merchantOperatingCityId language apiEntity.serviceTierShortDesc
    pure apiEntity {UEstimate.serviceTierName = serviceTierName, UEstimate.serviceTierShortDesc = serviceTierShortDesc}
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

getSpecialLocationTierOrder :: Maybe Text -> Maybe DRC.RiderConfig -> [DVST.ServiceTierType]
getSpecialLocationTierOrder Nothing _ = []
getSpecialLocationTierOrder (Just specialLocId) mbRiderConfig = fromMaybe [] $ do
  riderConfig <- mbRiderConfig
  configs <- riderConfig.specialLocationTierOrderConfig
  config <- find (\c -> c.specialLocationId == specialLocId) configs
  return config.orderArray

mostFrequentVehicleCategoryConfig :: Maybe DVST.ServiceTierType -> [VehicleServiceTierOrderConfig] -> [DVST.ServiceTierType]
mostFrequentVehicleCategoryConfig Nothing _ = []
mostFrequentVehicleCategoryConfig (Just vehicleServiceTier) configs =
  maybe [] (.orderArray) $ find (\v -> v.vehicle == vehicleServiceTier) configs

-- Sorting function
estimatesSorting :: [UEstimate.EstimateAPIEntity] -> [DVST.ServiceTierType] -> [DVST.ServiceTierType] -> [UEstimate.EstimateAPIEntity]
estimatesSorting list userOrder defaultOrder =
  let order = if null userOrder then defaultOrder else userOrder
   in sortBy (comparing (\estimate -> vehicleOrderIndex order estimate.serviceTierType)) list

quotesSorting :: [OfferRes] -> [DVST.ServiceTierType] -> [DVST.ServiceTierType] -> [OfferRes]
quotesSorting list userOrder defaultOrder =
  let order = if null userOrder then defaultOrder else userOrder
   in sortBy (comparing (offerVehicleOrderIndex order)) list
  where
    offerVehicleOrderIndex o = \case
      OnDemandCab q -> vehicleOrderIndex o q.vehicleVariant
      OnRentalCab q -> vehicleOrderIndex o q.vehicleVariant
      OnMeterRide q -> vehicleOrderIndex o q.vehicleVariant
      Metro _ -> maxBound
      PublicTransport _ -> maxBound

vehicleOrderIndex :: [DVST.ServiceTierType] -> DVST.ServiceTierType -> Int
vehicleOrderIndex order v =
  case lookup v (zip order [0 ..]) of
    Just idx -> idx
    Nothing -> maxBound
