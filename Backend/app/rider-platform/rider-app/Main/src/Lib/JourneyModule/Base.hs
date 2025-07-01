module Lib.JourneyModule.Base where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified BecknV2.FRFS.Enums as Spec
import Control.Monad.Extra (mapMaybeM)
import Data.List (sortBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Ord (comparing)
import qualified Data.Time as Time
import Domain.Action.UI.EditLocation as DEditLocation
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Action.UI.Location as DLoc
import Domain.Action.UI.Ride as DRide
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.CancellationReason as SCR
import Domain.Types.Extra.Ride as DRide
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSTicketBooking as DFRFSBooking
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.LocationAddress as LA
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RiderConfig
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.Trip as DTrip
import Environment
import EulerHS.Prelude (safeHead)
import Kernel.Beam.Functions
import Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface as KMultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.MultiModal.Interface.Types as KEMIT
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Common as Common
import Kernel.Types.Distance
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.JourneyLeg.Bus ()
import qualified Lib.JourneyLeg.Interface as JLI
import Lib.JourneyLeg.Metro ()
import Lib.JourneyLeg.Subway ()
import Lib.JourneyLeg.Taxi ()
import qualified Lib.JourneyLeg.Types as JL
import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Subway
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyLeg.Walk ()
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JL
import Lib.JourneyModule.Utils
import Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.GateInfo as GD
import qualified Sequelize as Se
import SharedLogic.Search
import qualified Storage.Beam.JourneyLeg as BJourneyLeg
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchOpCity
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyExtra as QJourneyExtra
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.Station as QStation
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.MultiModal as TMultiModal

filterTransitRoutes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => [MultiModalRoute] -> Id MerchantOperatingCity -> m [MultiModalRoute]
filterTransitRoutes routes mocid = do
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId mocid Nothing >>= fromMaybeM (RiderConfigDoesNotExist mocid.getId)
  if riderConfig.enableBusFiltering == Just True
    then filterM filterBusRoutes routes
    else return routes
  where
    filterBusRoutes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => MultiModalRoute -> m Bool
    filterBusRoutes route = do
      let legs = route.legs
          busLegs = filter (\leg -> leg.mode == MultiModalTypes.Bus) legs
      if null busLegs
        then return True
        else do
          busLegsValid <- forM busLegs $ \leg -> do
            case (leg.fromDepartureTime, leg.fromStopDetails >>= (.stopCode), leg.fromStopDetails >>= (.gtfsId)) of
              (Just departureTime, Just stopCode, Just routeId) -> do
                let buffer = 300 -- TODO: MOVE TO CONFIG.
                let departureTimeWithBuffer = buffer `addUTCTime` departureTime
                routeWithBuses <- CQMMB.getRoutesBuses routeId

                -- Check if the bus has an ETA for this stop
                return $
                  any
                    ( \bus -> do
                        -- vehicleRouteMapping <- QVehicleRouteMapping.findByVehicleNumber bus.vehicleNo
                        let busStopETA = find (\eta -> eta.stopCode == stopCode) (fromMaybe [] bus.busData.eta_data)
                        case busStopETA of
                          Just eta -> eta.arrivalTime > departureTimeWithBuffer
                          Nothing -> False
                    )
                    routeWithBuses.buses
              _ -> return False

          return (all (\x -> x) busLegsValid)

init ::
  JL.GetFareFlow m r =>
  JL.JourneyInitData ->
  APITypes.MultimodalUserPreferences ->
  m (Maybe DJourney.Journey)
init journeyReq userPreferences = do
  journeyId <- Common.generateGUID
  riderConfig <- QRC.findByMerchantOperatingCityId journeyReq.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist journeyReq.merchantOperatingCityId.getId)
  legsAndFares <-
    mapWithIndex
      ( \idx leg -> do
          let travelMode = convertMultiModalModeToTripMode leg.mode (straightLineDistance leg) (distanceToMeters leg.distance) journeyReq.maximumWalkDistance journeyReq.straightLineThreshold
          mbTotalLegFare <- measureLatency (JLI.getFare leg.fromArrivalTime journeyReq.personId journeyReq.merchantId journeyReq.merchantOperatingCityId leg travelMode) "multimodal getFare"
          if riderConfig.multimodalTesting
            then do
              journeyLeg <- JL.mkJourneyLeg idx leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance journeyReq.straightLineThreshold mbTotalLegFare
              QJourneyLeg.create journeyLeg
              return (mbTotalLegFare, Just journeyLeg)
            else case mbTotalLegFare of
              Just fare -> do
                journeyLeg <- JL.mkJourneyLeg idx leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance journeyReq.straightLineThreshold (Just fare)
                QJourneyLeg.create journeyLeg
                return (mbTotalLegFare, Just journeyLeg)
              Nothing -> return (Nothing, Nothing)
      )
      journeyReq.legs

  let (mbTotalFares, mbJourneyLegs) = unzip legsAndFares
  logDebug $ "[Multimodal - Legs]" <> show mbTotalFares
  if not riderConfig.multimodalTesting && (any isNothing mbTotalFares)
    then do return Nothing
    else do
      searchReq <- QSearchRequest.findById journeyReq.parentSearchId >>= fromMaybeM (SearchRequestNotFound journeyReq.parentSearchId.getId)
      let fromLocationAddress = UCommon.mkAddress searchReq.fromLocation.address
      let toLocationAddress = UCommon.mkAddress <$> (Just searchReq >>= (.toLocation) >>= (Just <$> (.address)))
      let journeyLegs = catMaybes mbJourneyLegs
      hasUserPreferredTransitTypesFlag <- hasUserPreferredTransitTypes journeyLegs userPreferences
      hasUserPreferredTransitModesFlag <- hasUserPreferredTransitModes journeyLegs userPreferences
      journey <- JL.mkJourney journeyReq.personId journeyReq.startTime journeyReq.endTime journeyReq.estimatedDistance journeyReq.estimatedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId journeyReq.legs journeyReq.maximumWalkDistance journeyReq.straightLineThreshold (searchReq.recentLocationId) journeyReq.relevanceScore hasUserPreferredTransitTypesFlag hasUserPreferredTransitModesFlag fromLocationAddress toLocationAddress
      QJourney.create journey
      logDebug $ "journey for multi-modal: " <> show journey
      return $ Just journey
  where
    straightLineDistance leg = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)
    hasUserPreferredTransitTypes legs userPrefs = do
      let relevantLegs = filter (\leg -> leg.mode == DTrip.Bus || leg.mode == DTrip.Subway) legs
          checkLeg leg =
            case leg.mode of
              DTrip.Bus -> checkTransitType userPrefs.busTransitTypes leg.serviceTypes
              DTrip.Subway -> checkTransitType userPrefs.subwayTransitTypes leg.serviceTypes
              _ -> True
      return (all checkLeg relevantLegs)
      where
        checkTransitType :: Maybe [Spec.ServiceTierType] -> Maybe [Spec.ServiceTierType] -> Bool
        checkTransitType userPreferredServiceTiers availableServiceTiers =
          case userPreferredServiceTiers of
            Nothing -> True
            Just preferred ->
              case availableServiceTiers of
                Nothing -> False
                Just types -> any (`elem` preferred) types
    hasUserPreferredTransitModes legs userPrefs = do
      return (all (\leg -> leg.mode `elem` userPrefs.allowedTransitModes) legs)

getJourney :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m DJourney.Journey
getJourney id = QJourney.findByPrimaryKey id >>= fromMaybeM (JourneyNotFound id.getId)

getJourneyLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m [DJourneyLeg.JourneyLeg]
getJourneyLegs journeyId = do
  legs <- QJourneyLeg.findAllByJourneyId journeyId
  let filteredLegs = filter (\leg -> leg.isDeleted == Just False || leg.isDeleted == Nothing) legs
  return $ sortBy (comparing (.sequenceNumber)) filteredLegs

getAllLegsInfo ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  Bool ->
  m [JL.LegInfo]
getAllLegsInfo journeyId skipAddLegFallback = do
  whenJourneyUpdateInProgress journeyId $ do
    allLegsRawData <- getJourneyLegs journeyId
    mapMaybeM
      ( \leg -> do
          case leg.legSearchId of
            Just legSearchIdText -> getLegInfo leg legSearchIdText
            Nothing -> do
              logError $ "LegId is null for JourneyLeg: " <> show leg.journeyId <> " JourneyLegId: " <> show leg.id
              if skipAddLegFallback
                then return Nothing
                else do
                  addAllLegs journeyId [leg]
                  updatedLeg <- QJourneyLeg.findByPrimaryKey leg.id >>= fromMaybeM (JourneyLegNotFound leg.id.getId)
                  legSearchIdText' <- updatedLeg.legSearchId & fromMaybeM (JourneyLegSearchIdNotFound leg.journeyId.getId leg.sequenceNumber)
                  getLegInfo updatedLeg legSearchIdText'
      )
      allLegsRawData
  where
    getLegInfo ::
      JL.GetStateFlow m r c =>
      DJourneyLeg.JourneyLeg ->
      Text ->
      m (Maybe JL.LegInfo)
    getLegInfo leg legSearchIdText = do
      let legSearchId = Id legSearchIdText
      case leg.mode of
        DTrip.Taxi -> JL.getInfo $ TaxiLegRequestGetInfo $ TaxiLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg = leg, ignoreOldSearchRequest = skipAddLegFallback}
        DTrip.Walk -> JL.getInfo $ WalkLegRequestGetInfo $ WalkLegRequestGetInfoData {walkLegId = cast legSearchId, journeyLeg = leg, ignoreOldSearchRequest = skipAddLegFallback}
        DTrip.Metro -> JL.getInfo $ MetroLegRequestGetInfo $ MetroLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare, distance = leg.distance, duration = leg.duration, journeyLeg = leg, ignoreOldSearchRequest = skipAddLegFallback}
        DTrip.Subway -> JL.getInfo $ SubwayLegRequestGetInfo $ SubwayLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare, distance = leg.distance, duration = leg.duration, journeyLeg = leg, ignoreOldSearchRequest = skipAddLegFallback}
        DTrip.Bus -> JL.getInfo $ BusLegRequestGetInfo $ BusLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare, distance = leg.distance, duration = leg.duration, journeyLeg = leg, ignoreOldSearchRequest = skipAddLegFallback}

hasSignificantMovement :: [LatLong] -> Domain.Types.RiderConfig.BusTrackingConfig -> Bool
hasSignificantMovement (p1 : p2 : _) busTrackingConfig =
  let d1 = highPrecMetersToMeters $ distanceBetweenInMeters p1 p2
   in d1 > (round busTrackingConfig.movementThresholdInMeters)
hasSignificantMovement _ _ = False

getRiderConfig :: (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) => DJourney.Journey -> m Domain.Types.RiderConfig.RiderConfig
getRiderConfig journey' = do
  case journey'.merchantOperatingCityId of
    Just merchantOperatingCityId ->
      QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId Nothing
        >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
    _ -> fromMaybeM (RiderConfigDoesNotExist "") Nothing

defaultBusTrackingConfig :: Domain.Types.RiderConfig.BusTrackingConfig
defaultBusTrackingConfig =
  Domain.Types.RiderConfig.BusTrackingConfig
    { fairScore = 4.0,
      fairScoreDistanceInMeters = 45.0,
      goodScore = 7.0,
      goodScoreDistanceInMeters = 30.0,
      maxScore = 10.0,
      maxScoreDistanceInMeters = 15.0,
      thresholdFactor = 0.5,
      thresholdSeconds = 30.0,
      movementThresholdInMeters = 25.0
    }

checkAndMarkJourneyAsFeedbackPending ::
  (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  DJourney.Journey ->
  [JL.JourneyLegState] ->
  m ()
checkAndMarkJourneyAsFeedbackPending journey allLegsState = do
  when
    ( all
        ( \st -> case st of
            JL.Single legState -> legState.status `elem` JL.allCompletedStatus
            JL.Transit legStates -> all (\legState -> legState.status `elem` JL.allCompletedStatus) legStates
        )
        allLegsState
        && journey.status /= DJourney.CANCELLED
    )
    $ updateJourneyStatus journey DJourney.FEEDBACK_PENDING

getAllLegsStatus ::
  (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  DJourney.Journey ->
  m [JL.JourneyLegState]
getAllLegsStatus journey = do
  allLegsRawData <- getJourneyLegs journey.id
  riderLastPoints <- getLastThreePoints journey.id
  riderConfig <- getRiderConfig journey
  let busTrackingConfig = fromMaybe defaultBusTrackingConfig riderConfig.busTrackingConfig
  let movementDetected = hasSignificantMovement (map (.latLong) riderLastPoints) busTrackingConfig
  (_, _, legPairs) <- foldlM (processLeg riderLastPoints movementDetected) (True, Nothing, []) allLegsRawData
  let allLegsState = map snd legPairs
  -- Update journey expiry time to the next valid ticket expiry when a leg is completed
  whenJust (minimumTicketLegOrder legPairs) $ \nextLegOrder -> do
    QJourneyExtra.updateJourneyToNextTicketExpiryTime journey.id nextLegOrder
  checkAndMarkJourneyAsFeedbackPending journey allLegsState
  return allLegsState
  where
    minimumTicketLegOrder = foldl' go Nothing
      where
        go acc (leg, legState)
          | leg.mode `elem` [DTrip.Walk, DTrip.Taxi] = acc -- Skip non-ticket modes
          | isIncomplete legState =
            case acc of
              Nothing -> Just (leg.sequenceNumber)
              Just minS -> Just (min minS leg.sequenceNumber)
          | otherwise = acc

        isIncomplete :: JL.JourneyLegState -> Bool
        isIncomplete (JL.Single legData) =
          legData.status `notElem` JL.allCompletedStatus
        isIncomplete (JL.Transit legDataList) =
          any (\legData -> legData.status `notElem` JL.allCompletedStatus) legDataList

    getRouteCodeToTrack :: DJourneyLeg.JourneyLeg -> Maybe Text
    getRouteCodeToTrack leg = safeHead leg.routeDetails >>= ((gtfsId :: KEMIT.MultiModalRouteDetails -> Maybe Text) >=> (pure . gtfsIdtoDomainCode))

    processLeg ::
      (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
      [APITypes.RiderLocationReq] ->
      Bool ->
      (Bool, Maybe DJourneyLeg.JourneyLeg, [(DJourneyLeg.JourneyLeg, JL.JourneyLegState)]) ->
      DJourneyLeg.JourneyLeg ->
      m (Bool, Maybe DJourneyLeg.JourneyLeg, [(DJourneyLeg.JourneyLeg, JL.JourneyLegState)])
    processLeg riderLastPoints movementDetected (isLastCompleted, lastLeg, legsState) leg = do
      case leg.legSearchId of
        Just legSearchIdText -> do
          let legSearchId = Id legSearchIdText
          legState <-
            case leg.mode of
              DTrip.Taxi -> JL.getState $ TaxiLegRequestGetState $ TaxiLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Walk -> JL.getState $ WalkLegRequestGetState $ WalkLegRequestGetStateData {walkLegId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Metro -> JL.getState $ MetroLegRequestGetState $ MetroLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Subway -> JL.getState $ SubwayLegRequestGetState $ SubwayLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Bus -> JL.getState $ BusLegRequestGetState $ BusLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted, movementDetected, routeCodeForDetailedTracking = getRouteCodeToTrack leg}
          return
            ( case legState of
                JL.Single legData -> legData.status == JL.Completed
                JL.Transit legDataList -> all (\legData -> legData.status == JL.Completed) legDataList,
              Just leg,
              legsState <> [(leg, legState)]
            )
        Nothing -> do
          logError $ "LegId is null for JourneyLeg: " <> show leg.journeyId <> " JourneyLegId: " <> show leg.id
          addAllLegs journey.id [leg] -- try to add the leg again
          updatedLeg <- QJourneyLeg.findByPrimaryKey leg.id >>= fromMaybeM (JourneyLegNotFound leg.id.getId)
          case updatedLeg.legSearchId of
            Just _ -> do
              processLeg riderLastPoints movementDetected (isLastCompleted, lastLeg, legsState) updatedLeg
            Nothing -> do
              throwError $ JourneyLegSearchIdNotFound leg.journeyId.getId leg.sequenceNumber

getMultiModalTransitOptions ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  APITypes.MultimodalUserPreferences ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  APITypes.MultimodalTransitOptionsReq ->
  m APITypes.MultimodalTransitOptionsResp
getMultiModalTransitOptions userPreferences merchantId merchantOperatingCityId req = do
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigNotFound merchantOperatingCityId.getId)
  -- let permissibleModesToUse = fromMaybe [] riderConfig.permissibleModes
  let permissibleModesToUse =
        if null userPreferences.allowedTransitModes
          then fromMaybe [] riderConfig.permissibleModes
          else userPreferencesToGeneralVehicleTypes userPreferences.allowedTransitModes
  now <- getCurrentTime
  let transitRoutesReq =
        GetTransitRoutesReq
          { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = req.sourceLatLong.lat, longitude = req.sourceLatLong.lon}}},
            destination = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = req.destLatLong.lat, longitude = req.destLatLong.lon}}},
            arrivalTime = Nothing,
            departureTime = Just now,
            mode = Nothing,
            transitPreferences = Nothing,
            transportModes = Nothing,
            minimumWalkDistance = riderConfig.minimumWalkDistance,
            permissibleModes = permissibleModesToUse,
            maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
            sortingType = convertSortingType $ fromMaybe DMP.FASTEST userPreferences.journeyOptionsSortingType
          }
  transitServiceReq <- TMultiModal.getTransitServiceReq merchantId merchantOperatingCityId
  otpResponse <- KMultiModal.getTransitRoutes Nothing transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError "routes dont exist")
  logDebug $ "[getMultiModalTransitOptions - OTP Response]" <> show otpResponse

  let processedRoutes = map (processRoute riderConfig.maximumWalkDistance) otpResponse.routes
  let transitOptions = map extractOptionFromRoute processedRoutes

  return $
    APITypes.MultimodalTransitOptionsResp
      { options = transitOptions
      }
  where
    -- Process a route to convert walk legs that exceed maximum distance to taxi
    processRoute :: Meters -> MultiModalRoute -> MultiModalRoute
    processRoute maxWalkDistance route =
      route {legs = map (processLeg maxWalkDistance) route.legs}

    -- Process a leg to check if walk mode needs to be converted to taxi
    processLeg :: Meters -> MultiModalLeg -> MultiModalLeg
    processLeg maxWalkDistance leg =
      if leg.mode == MultiModalTypes.Walk && distanceToMeters leg.distance > maxWalkDistance
        then leg {mode = MultiModalTypes.Unspecified} -- Use Unspecified to represent Taxi (similar to init function)
        else leg

    extractOptionFromRoute :: MultiModalRoute -> APITypes.MultimodalTransitOptionData
    extractOptionFromRoute route =
      APITypes.MultimodalTransitOptionData
        { duration = Just route.duration,
          travelModes = concatMap legToAllowedTransitModes route.legs
        }

    legToAllowedTransitModes :: MultiModalLeg -> [DTrip.MultimodalTravelMode]
    legToAllowedTransitModes leg =
      case generalVehicleTypeToAllowedTransitMode leg.mode of
        Just mode -> [mode]
        Nothing -> []

    generalVehicleTypeToAllowedTransitMode :: GeneralVehicleType -> Maybe DTrip.MultimodalTravelMode
    generalVehicleTypeToAllowedTransitMode vehicleType = case vehicleType of
      MultiModalTypes.Bus -> Just DTrip.Bus
      MultiModalTypes.MetroRail -> Just DTrip.Metro
      MultiModalTypes.Subway -> Just DTrip.Subway
      MultiModalTypes.Walk -> Just DTrip.Walk
      _ -> Nothing

    userPreferencesToGeneralVehicleTypes :: [DTrip.MultimodalTravelMode] -> [GeneralVehicleType]
    userPreferencesToGeneralVehicleTypes = mapMaybe allowedTransitModeToGeneralVehicleType

    allowedTransitModeToGeneralVehicleType :: DTrip.MultimodalTravelMode -> Maybe GeneralVehicleType
    allowedTransitModeToGeneralVehicleType mode = case mode of
      DTrip.Bus -> Just MultiModalTypes.Bus
      DTrip.Metro -> Just MultiModalTypes.MetroRail
      DTrip.Subway -> Just MultiModalTypes.Subway
      DTrip.Walk -> Just MultiModalTypes.Walk
      _ -> Nothing

startJourney ::
  (JL.ConfirmFlow m r c, JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  [APITypes.JourneyConfirmReqElement] ->
  Maybe Int ->
  Id DJourney.Journey ->
  m ()
startJourney confirmElements forcedBookedLegOrder journeyId = do
  allLegs <- getAllLegsInfo journeyId False
  mapM_
    ( \leg -> do
        let mElement = find (\element -> element.journeyLegOrder == leg.order) confirmElements
            ticketQuantity = mElement >>= (.ticketQuantity)
            childTicketQuantity = mElement >>= (.childTicketQuantity)
        let forcedBooking = Just leg.order == forcedBookedLegOrder
        let crisSdkResponse = find (\element -> element.journeyLegOrder == leg.order) confirmElements >>= (.crisSdkResponse)
        when (isJust crisSdkResponse) $ logInfo $ "CRIS: crisSdkResponse startJourney: " <> show crisSdkResponse
        when (leg.status /= JL.Skipped) $ do
          JLI.confirm forcedBooking ticketQuantity childTicketQuantity leg crisSdkResponse
    )
    allLegs

addAllLegs ::
  ( JL.SearchRequestFlow m r c,
    JL.GetStateFlow m r c,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  Id DJourney.Journey ->
  [DJourneyLeg.JourneyLeg] ->
  m ()
addAllLegs journeyId journeyLegs = do
  journey <- getJourney journeyId
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (SearchRequestNotFound journey.searchRequestId.getId)
  oldLegs <- getJourneyLegs journeyId
  let filteredOldLegs = filter (\leg1 -> all (\leg2 -> not (leg1.sequenceNumber == leg2.sequenceNumber)) journeyLegs) oldLegs
  let allLegs = sortBy (comparing (.sequenceNumber)) (filteredOldLegs ++ journeyLegs)
  toLocation <- parentSearchReq.toLocation & fromMaybeM (InvalidRequest "To location nothing for parent search request")
  forM_ (traverseWithTriplets allLegs) $ \(mbPrevJourneyLeg, journeyLeg, mbNextJourneyLeg) -> do
    when (isNothing journeyLeg.legSearchId) $ do
      -- In case of retry of this function, if search has already triggered then it will not do it again
      searchResp <-
        case journeyLeg.mode of
          DTrip.Taxi -> do
            snappedLeg <- snapJourneyLegToNearestGate journeyLeg
            let originAddress = mkAddress (mbPrevJourneyLeg >>= (.toStopDetails)) parentSearchReq.fromLocation.address
            let destinationAddress = mkAddress (mbNextJourneyLeg >>= (.fromStopDetails)) toLocation.address
            addTaxiLeg parentSearchReq snappedLeg originAddress destinationAddress
          DTrip.Metro -> do
            addMetroLeg parentSearchReq journeyLeg
          DTrip.Subway -> do
            addSubwayLeg parentSearchReq journeyLeg
          DTrip.Walk -> do
            let originAddress = mkAddress (mbPrevJourneyLeg >>= (.toStopDetails)) parentSearchReq.fromLocation.address
            let destinationAddress = mkAddress (mbNextJourneyLeg >>= (.fromStopDetails)) toLocation.address
            addWalkLeg parentSearchReq journeyLeg originAddress destinationAddress
          DTrip.Bus -> do
            addBusLeg parentSearchReq journeyLeg
      upsertJourneyLeg $ journeyLeg {DJourneyLeg.legSearchId = Just searchResp.id}
  where
    traverseWithTriplets :: [a] -> [(Maybe a, a, Maybe a)]
    traverseWithTriplets [] = []
    traverseWithTriplets [x] = [(Nothing, x, Nothing)] -- Single element case
    traverseWithTriplets xs = go Nothing xs
      where
        go _ [] = []
        go prev [x] = [(prev, x, Nothing)] -- Last element case
        go prev (x : y : rest) = (prev, x, Just y) : go (Just x) (y : rest)

    mkAddress :: Maybe MultiModalStopDetails -> LA.LocationAddress -> LA.LocationAddress
    mkAddress Nothing parentAddress = parentAddress
    mkAddress (Just stopDetails) _ =
      LA.LocationAddress
        { street = Nothing,
          door = stopDetails.name,
          city = Nothing,
          state = Nothing,
          country = Nothing,
          building = Nothing,
          areaCode = Nothing,
          area = stopDetails.name,
          ward = Nothing,
          placeId = Nothing,
          instructions = Nothing,
          title = stopDetails.name,
          extras = Nothing
        }

snapJourneyLegToNearestGate ::
  ( JL.SearchRequestFlow m r c
  ) =>
  DJourneyLeg.JourneyLeg ->
  m DJourneyLeg.JourneyLeg
snapJourneyLegToNearestGate journeyLeg = do
  mbSpecialLocationStart <- Esq.runInReplica $ QSpecialLocation.findSpecialLocationByLatLongFull (LatLong (journeyLeg.startLocation.latitude) (journeyLeg.startLocation.longitude))
  snappedStart <- case mbSpecialLocationStart of
    Just specialLoc -> do
      let mbFilteredSpecialLoc = QSpecialLocation.filterGates (Just specialLoc) True
      case mbFilteredSpecialLoc of
        Just filteredSpecialLoc ->
          if null (gatesInfo filteredSpecialLoc)
            then return journeyLeg.startLocation
            else findNearestGate journeyLeg.startLocation journeyLeg.endLocation (gatesInfo filteredSpecialLoc)
        Nothing -> return journeyLeg.startLocation
    Nothing -> return journeyLeg.startLocation
  return journeyLeg {DJourneyLeg.startLocation = snappedStart}
  where
    findNearestGate ::
      ( JL.SearchRequestFlow m r c
      ) =>
      LatLngV2 ->
      LatLngV2 ->
      [GD.GateInfoFull] ->
      m LatLngV2
    findNearestGate startLocation destLocation gates = do
      case nonEmpty gates of
        Nothing -> return startLocation
        Just nonEmptyGates -> do
          merchantId <- journeyLeg.merchantId & fromMaybeM (InvalidRequest $ "MerchantId not found for journeyLegId: " <> journeyLeg.id.getId)
          merchantOperatingCityId <- journeyLeg.merchantOperatingCityId & fromMaybeM (InvalidRequest $ "MerchantOperatingCityId not found for journeyLegId: " <> journeyLeg.id.getId)
          let destinations = LatLong (destLocation.latitude) (destLocation.longitude) :| []
          let originLocs = fmap (.point) nonEmptyGates
          distanceResponses <-
            Maps.getMultimodalJourneyDistances merchantId merchantOperatingCityId (Just journeyLeg.id.getId) $
              Maps.GetDistancesReq
                { origins = originLocs,
                  destinations = destinations,
                  travelMode = Just Maps.CAR,
                  sourceDestinationMapping = Nothing,
                  distanceUnit = Meter
                }
          let nearestResp = minimumBy (compare `on` (.distance)) (toList distanceResponses)
          let nearestGateLocation =
                fromMaybe destLocation $
                  fmap (\g -> LatLngV2 (g.point.lat) (g.point.lon)) $
                    find (\g -> g.point == nearestResp.origin) (toList nonEmptyGates)
          return nearestGateLocation

addTaxiLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  LA.LocationAddress ->
  LA.LocationAddress ->
  m JL.SearchResponse
addTaxiLeg parentSearchReq journeyLeg originAddress destinationAddress = do
  let startLocation = JL.mkSearchReqLocation originAddress journeyLeg.startLocation
  let endLocation = JL.mkSearchReqLocation destinationAddress journeyLeg.endLocation
  let taxiSearchReq = mkTaxiSearchReq startLocation [endLocation]
  JL.search taxiSearchReq
  where
    mkTaxiSearchReq :: SearchReqLocation -> [SearchReqLocation] -> TaxiLegRequest
    mkTaxiSearchReq origin stops =
      TaxiLegRequestSearch $
        TaxiLegRequestSearchData
          { journeyLegData = journeyLeg,
            ..
          }

addWalkLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  LA.LocationAddress ->
  LA.LocationAddress ->
  m JL.SearchResponse
addWalkLeg parentSearchReq journeyLeg originAddress destinationAddress = do
  let startLocation = JL.mkSearchReqLocation originAddress journeyLeg.startLocation
  let endLocation = JL.mkSearchReqLocation destinationAddress journeyLeg.endLocation
  let walkSearchReq = mkWalkSearchReq startLocation endLocation
  JL.search walkSearchReq
  where
    mkWalkSearchReq :: SearchReqLocation -> SearchReqLocation -> WalkLegRequest
    mkWalkSearchReq origin destination =
      WalkLegRequestSearch $
        WalkLegRequestSearchData
          { journeyLegData = journeyLeg,
            ..
          }

addMetroLeg ::
  (JL.SearchRequestFlow m r c, JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addMetroLeg parentSearchReq journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById parentSearchReq.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound parentSearchReq.merchantOperatingCityId.getId)
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
  let metroSearchReq = mkMetroLegReq merchantOperatingCity.city
  searchResp <- JL.search metroSearchReq

  now <- getCurrentTime
  shouldSkip <- case (riderConfig.qrTicketRestrictionStartTime, riderConfig.qrTicketRestrictionEndTime) of
    (Just startTime, Just endTime) -> do
      let isOutsideRestrictedHours = isOutsideRestrictionTime startTime endTime now riderConfig.timeDiffFromUtc
      let isMetroBookingAllowed = fromMaybe True riderConfig.metroBookingAllowed
      return $ not (isOutsideRestrictedHours && isMetroBookingAllowed)
    _ -> do
      -- No restriction times set, check only metroBookingAllowed
      let isMetroBookingAllowed = fromMaybe True riderConfig.metroBookingAllowed
      return $ not isMetroBookingAllowed

  when shouldSkip $ do
    let reason =
          if not (fromMaybe True riderConfig.metroBookingAllowed)
            then "metro booking not allowed"
            else "restricted hours"
    logInfo $ "Marking Metro leg as skipped due to " <> reason <> ". Current time: " <> show now
    skipLeg journeyLeg.journeyId journeyLeg.sequenceNumber False

  return searchResp
  where
    mkMetroLegReq city = do
      MetroLegRequestSearch $
        MetroLegRequestSearchData
          { quantity = 1,
            personId = parentSearchReq.riderId,
            merchantId = parentSearchReq.merchantId,
            recentLocationId = parentSearchReq.recentLocationId,
            city,
            journeyLeg
          }

addSubwayLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addSubwayLeg parentSearchReq journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById parentSearchReq.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound parentSearchReq.merchantOperatingCityId.getId)
  let subwaySearchReq = mkSubwayLegReq merchantOperatingCity.city
  JL.search subwaySearchReq
  where
    mkSubwayLegReq city = do
      SubwayLegRequestSearch $
        SubwayLegRequestSearchData
          { quantity = 1,
            personId = parentSearchReq.riderId,
            merchantId = parentSearchReq.merchantId,
            recentLocationId = parentSearchReq.recentLocationId,
            city,
            journeyLeg
          }

addBusLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addBusLeg parentSearchReq journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById parentSearchReq.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound parentSearchReq.merchantOperatingCityId.getId)
  let busSearchReq = mkBusLegReq merchantOperatingCity.city
  JL.search busSearchReq
  where
    mkBusLegReq city = do
      BusLegRequestSearch $
        BusLegRequestSearchData
          { quantity = 1,
            personId = parentSearchReq.riderId,
            merchantId = parentSearchReq.merchantId,
            recentLocationId = parentSearchReq.recentLocationId,
            city,
            journeyLeg
          }

isOutsideRestrictionTime :: Time.TimeOfDay -> Time.TimeOfDay -> UTCTime -> Seconds -> Bool
isOutsideRestrictionTime startTime endTime now timeDiffFromUtc =
  let tzMinutes = getSeconds timeDiffFromUtc `div` 60
      tz = Time.minutesToTimeZone tzMinutes
      nowAsLocal = Time.utcToLocalTime tz now
      nowTOD = Time.localTimeOfDay nowAsLocal

      --handle midnight wrap
      inWindow =
        if startTime <= endTime
          then nowTOD >= startTime && nowTOD <= endTime
          else nowTOD >= startTime || nowTOD <= endTime
   in not inWindow

getRemainingLegs ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getRemainingLegs journeyId = do
  journeyLegs <- getAllLegsInfo journeyId False
  let remainingLegs = filter cancellableStatus journeyLegs -- check if edge case is to be handled [completed , skipped, inplan]
  return remainingLegs

getRemainingLegsForExtend ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getRemainingLegsForExtend journeyId = do
  journeyLegs <- getAllLegsInfo journeyId False
  let remainingLegs = filter cancellableExtendStatus journeyLegs
  return remainingLegs

cancellableExtendStatus :: JL.LegInfo -> Bool
cancellableExtendStatus leg = if leg.travelMode == DTrip.Walk then not (leg.status `elem` JL.cannotCancelWalkStatus) else not (leg.status `elem` JL.cannotCancelExtendStatus)

cancellableStatus :: JL.LegInfo -> Bool
cancellableStatus leg = if leg.travelMode == DTrip.Walk then not (leg.status `elem` JL.cannotCancelWalkStatus) else not (leg.status `elem` JL.cannotCancelStatus)

getUnifiedQR :: DJourney.Journey -> [JL.LegInfo] -> Maybe JL.UnifiedTicketQR
getUnifiedQR journey legs = do
  let bookings = mapMaybe getTickets (filter (\leg -> leg.travelMode `elem` [DTrip.Metro, DTrip.Bus, DTrip.Subway]) legs)
  let cmrlBookings = [b | (provider, b) <- bookings, provider == providerToText JL.CMRL]
  let mtcBookings = [b | (provider, b) <- bookings, provider == providerToText JL.MTC]
  let crisBookings = [b | (provider, b) <- bookings, provider == providerToText JL.CRIS]
  if null cmrlBookings && null mtcBookings && null crisBookings
    then Nothing
    else
      Just $
        JL.UnifiedTicketQR
          { version = "1.0",
            _type = "INTEGRATED_QR",
            txnId = journey.id.getId,
            createdAt = journey.createdAt,
            cmrl = cmrlBookings,
            mtc = mtcBookings,
            cris = crisBookings
          }

providerToText :: JL.Provider -> Text
providerToText JL.CMRL = "Chennai Metro Rail Limited"
providerToText JL.MTC = "Buses"
providerToText JL.DIRECT = "Direct Multimodal Services"
providerToText JL.CRIS = "CRIS Subway"

getTickets :: JL.LegInfo -> Maybe (Text, JL.BookingData)
getTickets leg =
  leg.pricingId >>= \_ -> do
    case leg.legExtraInfo of
      JL.Metro info -> processTickets JL.CMRL info.bookingId info.tickets info.providerName
      JL.Bus info -> processTickets JL.MTC info.bookingId info.tickets info.providerName
      JL.Subway info -> processTickets JL.CRIS info.bookingId info.tickets info.providerName
      _ -> Nothing
  where
    processTickets :: JL.Provider -> Maybe (Id DFRFSBooking.FRFSTicketBooking) -> Maybe [Text] -> Maybe Text -> Maybe (Text, JL.BookingData)
    processTickets expectedProvider mbBookingId mbTickets mbProviderName = do
      tickets <- mbTickets
      provider <- mbProviderName
      bookingId <- mbBookingId
      if (provider == providerToText expectedProvider || provider == providerToText JL.DIRECT) && not (null tickets)
        then
          Just
            ( providerToText expectedProvider,
              JL.BookingData
                { bookingId = bookingId.getId,
                  isRoundTrip = False, -- TODO: add round trip support
                  ticketData = tickets
                }
            )
        else Nothing

cancelLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  JL.LegInfo ->
  SCR.CancellationReasonCode ->
  Bool ->
  Bool ->
  m ()
cancelLeg journeyLeg cancellationReasonCode isSkipped skippedDuringConfirmation = do
  unless skippedDuringConfirmation $ do
    isCancellable <- checkIfCancellable journeyLeg
    unless isCancellable $
      throwError $ InvalidRequest $ "Cannot cancel leg for: " <> show journeyLeg.travelMode
  case journeyLeg.travelMode of
    DTrip.Taxi ->
      JL.cancel $
        TaxiLegRequestCancel
          TaxiLegRequestCancelData
            { searchRequestId = Id journeyLeg.searchId,
              reasonCode = cancellationReasonCode,
              additionalInfo = Nothing,
              reallocate = Nothing,
              blockOnCancellationRate = Nothing,
              cancellationSource = SBCR.ByUser,
              isSkipped
            }
    DTrip.Walk ->
      JL.cancel $
        WalkLegRequestCancel
          WalkLegRequestCancelData
            { walkLegId = Id journeyLeg.searchId
            }
    DTrip.Metro ->
      JL.cancel $
        MetroLegRequestCancel
          MetroLegRequestCancelData
            { searchId = Id journeyLeg.searchId,
              cancellationType = Spec.CONFIRM_CANCEL,
              isSkipped
            }
    DTrip.Subway ->
      JL.cancel $
        SubwayLegRequestCancel
          SubwayLegRequestCancelData
            { searchId = Id journeyLeg.searchId,
              cancellationType = Spec.CONFIRM_CANCEL,
              isSkipped
            }
    DTrip.Bus ->
      JL.cancel $
        BusLegRequestCancel
          BusLegRequestCancelData
            { searchId = Id journeyLeg.searchId,
              cancellationType = Spec.CONFIRM_CANCEL,
              isSkipped
            }
  return ()

cancelRemainingLegs ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  Bool ->
  m ()
cancelRemainingLegs journeyId isExtend = do
  remainingLegs <- if isExtend then (getRemainingLegsForExtend journeyId) else (getRemainingLegs journeyId)
  -- forM_ remainingLegs $ \leg -> do
  --   isCancellable <- checkIfCancellable leg
  --   unless isCancellable $
  --     throwError $ InvalidRequest $ "Cannot cancel leg for leg: " <> show leg.travelMode
  results <-
    forM remainingLegs $ \leg -> do
      try @_ @SomeException $
        if leg.skipBooking
          then return ()
          else do
            isCancellable <- checkIfCancellable leg
            if isCancellable then (cancelLeg leg (SCR.CancellationReasonCode "")) False False else (QJourneyLeg.updateIsDeleted (Just True) (Just leg.searchId))
  let failures = [e | Left e <- results]
  unless (null failures) $
    throwError $ InvalidRequest $ "Failed to cancel some legs: " <> show failures

multimodalLegSearchIdAccessLockKey :: Text -> Text
multimodalLegSearchIdAccessLockKey legSearchId = "Multimodal:Leg:SearchIdAccess:" <> legSearchId

skipLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  Int ->
  Bool ->
  m ()
skipLeg journeyId legOrder skippedDuringConfirmation = do
  allLegs <- getAllLegsInfo journeyId False
  skippingLeg <- fromMaybeM (InvalidRequest $ "Leg not found: " <> show legOrder) $ find (\leg -> leg.order == legOrder) allLegs
  if skippingLeg.skipBooking
    then return ()
    else do
      when (skippingLeg.travelMode == DTrip.Walk) $
        throwError $ JourneyLegCannotBeSkippedForMode (show skippingLeg.travelMode)
      unless (cancellableStatus skippingLeg) $
        throwError $ JourneyLegCannotBeSkippedForStatus (show skippingLeg.status)
      cancelLeg skippingLeg (SCR.CancellationReasonCode "") True skippedDuringConfirmation

addSkippedLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  Int ->
  m ()
addSkippedLeg journeyId legOrder = do
  allLegs <- QJourneyLeg.findAllByJourneyId journeyId
  skippedLeg <-
    find
      (\leg -> leg.isSkipped == Just True && leg.sequenceNumber == legOrder && leg.mode /= DTrip.Walk)
      allLegs
      & fromMaybeM (InvalidRequest $ "Skipped Leg not found with leg Order: " <> show legOrder)

  legSearchId <-
    skippedLeg.legSearchId
      & fromMaybeM (InvalidRequest "legSearchId is missing for skippedLeg")

  isSkippedOrCancelled <- case skippedLeg.mode of
    DTrip.Taxi -> do
      mbBooking <- QBooking.findByTransactionId legSearchId
      case mbBooking of
        Just booking -> return booking.isSkipped
        Nothing -> do
          searchReq <- QSearchRequest.findById (Id legSearchId) >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legSearchId)
          case searchReq.journeyLegInfo of
            Just journeyData -> return (Just journeyData.skipBooking)
            Nothing -> return Nothing
    DTrip.Metro -> checkFRFSBooking legSearchId
    DTrip.Subway -> checkFRFSBooking legSearchId
    DTrip.Bus -> checkFRFSBooking legSearchId
    _ -> throwError $ InvalidRequest $ "Invalid mode, cannot skip this leg " <> show skippedLeg.mode

  when (isSkippedOrCancelled /= Just True) $
    throwError $ InvalidRequest $ "isSkipped is not True for legOrder: " <> show legOrder

  exep <- try @_ @SomeException $ do
    QJourneyLeg.updateIsSkipped (Just False) skippedLeg.legSearchId
    addAllLegs journeyId [skippedLeg {DJourneyLeg.isSkipped = Just False, DJourneyLeg.legSearchId = Nothing}]
  case exep of
    Left _ -> do
      -- Rollback operations
      QJourneyLeg.updateLegSearchId skippedLeg.legSearchId skippedLeg.id
      QJourneyLeg.updateIsSkipped (Just True) skippedLeg.legSearchId
      throwError $ InvalidRequest "Failed to update skipped leg, as Search operation failed"
    Right _ -> return ()
  where
    checkFRFSBooking legSearchId = do
      frfsBooking <- QTBooking.findBySearchId (Id legSearchId)
      case frfsBooking of
        Just booking -> return booking.isSkipped
        Nothing -> do
          frfsSearchReq <- QFRFSSearch.findById (Id legSearchId) >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legSearchId)
          case frfsSearchReq.journeyLegInfo of
            Just frfsJourneyData -> return (Just frfsJourneyData.skipBooking)
            Nothing -> return Nothing

checkIfCancellable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  JL.LegInfo ->
  m Bool
checkIfCancellable journeyLeg = do
  isCancellableResult <- case journeyLeg.travelMode of
    DTrip.Taxi ->
      JL.isCancellable $
        TaxiLegRequestIsCancellable
          TaxiLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId
            }
    DTrip.Walk ->
      JL.isCancellable $
        WalkLegRequestIsCancellable
          WalkLegRequestIsCancellableData
            { walkLegId = Id journeyLeg.searchId
            }
    DTrip.Metro ->
      JL.isCancellable $
        MetroLegRequestIsCancellable
          MetroLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId
            }
    DTrip.Subway -> JL.isCancellable $ SubwayLegRequestIsCancellable SubwayLegRequestIsCancellableData
    DTrip.Bus ->
      JL.isCancellable $
        BusLegRequestIsCancellable
          BusLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId
            }
  return isCancellableResult.canCancel

canBeSwitched ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    MonadFlow m
  ) =>
  JL.LegInfo ->
  DTrip.MultimodalTravelMode ->
  Maybe Distance ->
  m Bool
canBeSwitched legToBeSwitched newMode newDistance = do
  let currentMode = legToBeSwitched.travelMode
  case (currentMode, newMode) of
    (_, DTrip.Metro) -> return False
    (_, DTrip.Bus) -> return False
    (DTrip.Bus, DTrip.Taxi) -> return False
    (DTrip.Metro, DTrip.Taxi) -> return False
    (DTrip.Walk, DTrip.Taxi) -> return True
    (DTrip.Taxi, DTrip.Walk) -> do
      case newDistance of
        Just distance ->
          if getMeters (distanceToMeters distance) <= 2000
            then do return True
            else do throwError $ InvalidRequest "Can't switch to walk if distance is more than 2km, skip the ride instead"
        Nothing -> return True
    _ -> return False

updateJourneyStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  DJourney.Journey ->
  DJourney.JourneyStatus ->
  m ()
updateJourneyStatus journey newStatus = do
  when (newStatus > journey.status) $
    QJourney.updateStatus newStatus journey.id

isExtendable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  [JL.LegInfo] ->
  JL.LegInfo ->
  DTrip.MultimodalTravelMode ->
  m Bool
isExtendable allLegs currentLegInfo newMode = do
  let previousLegInfo = find (\leg -> leg.order == (currentLegInfo.order - 1)) allLegs
      nextLegInfo = find (\leg -> leg.order == (currentLegInfo.order + 1)) allLegs
  let startLeg =
        case previousLegInfo of
          Just prevLeg | prevLeg.travelMode == newMode -> prevLeg
          _ -> currentLegInfo
      endLeg =
        case nextLegInfo of
          Just nextLeg | nextLeg.travelMode == newMode -> nextLeg
          _ -> currentLegInfo
   in return (startLeg.order /= currentLegInfo.order || endLeg.order /= currentLegInfo.order)

createJourneyLegFromCancelledLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  DJourneyLeg.JourneyLeg ->
  DTrip.MultimodalTravelMode ->
  Maps.LatLngV2 ->
  Maybe Distance ->
  Maybe Seconds ->
  m DJourneyLeg.JourneyLeg
createJourneyLegFromCancelledLeg journeyLeg newMode startLocation newDistance newDuration = do
  now <- getCurrentTime
  journeyLegId <- generateGUID

  return $
    DJourneyLeg.JourneyLeg
      { agency = Nothing,
        distance = newDistance,
        duration = newDuration,
        endLocation = journeyLeg.endLocation,
        fromArrivalTime = Nothing,
        fromDepartureTime = Nothing,
        fromStopDetails = Nothing,
        id = journeyLegId,
        journeyId = journeyLeg.journeyId,
        mode = newMode,
        routeDetails = [],
        serviceTypes = Nothing,
        sequenceNumber = journeyLeg.sequenceNumber,
        startLocation = startLocation,
        toArrivalTime = Nothing,
        toDepartureTime = journeyLeg.toDepartureTime,
        toStopDetails = journeyLeg.toStopDetails,
        estimatedMinFare = Nothing,
        estimatedMaxFare = Nothing,
        merchantId = journeyLeg.merchantId,
        merchantOperatingCityId = journeyLeg.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        isDeleted = Just False,
        legSearchId = Nothing,
        isSkipped = Just False,
        changedBusesInSequence = journeyLeg.changedBusesInSequence,
        finalBoardedBusNumber = journeyLeg.finalBoardedBusNumber,
        entrance = journeyLeg.entrance,
        exit = journeyLeg.exit
      }

extendLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  JL.ExtendLegStartPoint ->
  Maybe DLocation.LocationAPIEntity ->
  Maybe Int ->
  JL.GetFareResponse ->
  Distance ->
  Seconds ->
  Maybe (Id DBUR.BookingUpdateRequest) ->
  m ()
extendLeg journeyId startPoint mbEndLocation mbEndLegOrder fare newDistance newDuration bookingUpdateReqId = do
  journey <- getJourney journeyId
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (SearchRequestNotFound journey.searchRequestId.getId)
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for searchId: " <> show parentSearchReq.id.getId) parentSearchReq.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation
  allLegs <- getAllLegsInfo journeyId False
  case startPoint of
    JL.StartLegOrder startLegOrder -> do
      currentLeg <- find (\leg -> leg.order == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)

      legsToCancel <-
        case mbEndLegOrder of
          Just endLegOrder -> return $ filter (\leg -> leg.order >= startLegOrder && leg.order < endLegOrder) allLegs
          Nothing -> return $ filter (\leg -> leg.order >= startLegOrder) allLegs
      -- checkIfRemainingLegsAreCancellable legsToCancel
      (newOriginLat, newOriginLon) <- getNewOriginLatLon currentLeg.legExtraInfo
      leg <- mkMultiModalLeg newDistance newDuration MultiModalTypes.Unspecified newOriginLat newOriginLon endLocation.lat endLocation.lon currentLeg.startTime
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg startLegOrder leg currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance riderConfig.straightLineThreshold (Just fare)
      startLocationAddress <-
        case currentLeg.legExtraInfo of
          JL.Walk walkLegExtraInfo -> return walkLegExtraInfo.origin.address
          JL.Taxi taxiLegExtraInfo -> return taxiLegExtraInfo.origin.address
          _ -> do
            frfsSearchReq <- QFRFSSearch.findById (Id currentLeg.searchId) >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> currentLeg.searchId)
            fromStation <- QStation.findById frfsSearchReq.fromStationId >>= fromMaybeM (InvalidRequest $ "from station not found in extendLeg: " <> show frfsSearchReq.fromStationId)
            return $ mkAddressFromStation fromStation.name
      withJourneyUpdateInProgress journeyId $ do
        forM_ legsToCancel $ \currLeg -> do
          isCancellable <- checkIfCancellable currLeg
          if isCancellable
            then cancelLeg currLeg (SCR.CancellationReasonCode "") False False
            else QJourneyLeg.updateIsDeleted (Just True) (Just currLeg.searchId)
        QJourneyLeg.create journeyLeg
        updateJourneyChangeLogCounter journeyId
        searchResp <- addTaxiLeg parentSearchReq journeyLeg startLocationAddress (mkLocationAddress endLocation)
        QJourneyLeg.updateLegSearchId (Just searchResp.id) journeyLeg.id
        when (currentLeg.status /= JL.InPlan) $
          fork "Start journey thread" $ withShortRetry $ startJourney [] Nothing journeyId
    JL.StartLocation startlocation -> do
      currentLeg <- find (\leg -> leg.order == startlocation.legOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startlocation.legOrder)
      case (currentLeg.travelMode, currentLeg.skipBooking) of
        (DTrip.Taxi, False) -> do
          bookingUpdateRequestId <- bookingUpdateReqId & fromMaybeM (InvalidRequest "bookingUpdateReqId not found")
          journeyLeg <- QJourneyLeg.findByLegSearchId (Just currentLeg.searchId) >>= fromMaybeM (InvalidRequest $ "JourneyLeg not found for searchId: " <> currentLeg.searchId)
          void $ DEditLocation.postEditResultConfirm (Just parentSearchReq.riderId, parentSearchReq.merchantId) bookingUpdateRequestId
          Redis.setExp mkExtendLegKey journeyLeg.id 300 --5 mins
        (DTrip.Taxi, True) -> extendWalkLeg startlocation endLocation currentLeg parentSearchReq
        (DTrip.Walk, _) -> extendWalkLeg startlocation endLocation currentLeg parentSearchReq
        _ -> do
          throwError $ InvalidRequest ("Cannot extend leg for mode: " <> show currentLeg.travelMode)
  where
    extendWalkLeg startlocation endLocation currentLeg parentSearchReq = do
      now <- getCurrentTime
      leg <- mkMultiModalLeg newDistance newDuration MultiModalTypes.Unspecified startlocation.location.lat startlocation.location.lon endLocation.lat endLocation.lon now
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg currentLeg.order leg currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance riderConfig.straightLineThreshold (Just fare)
      withJourneyUpdateInProgress journeyId $ do
        cancelRequiredLegs
        QJourneyLeg.create journeyLeg
        searchResp <- addTaxiLeg parentSearchReq journeyLeg (mkLocationAddress startlocation.location) (mkLocationAddress endLocation)
        QJourneyLeg.updateLegSearchId (Just searchResp.id) journeyLeg.id
        startJourney [] (Just currentLeg.order) journeyId

    cancelRequiredLegs = do
      case mbEndLegOrder of
        Nothing -> cancelRemainingLegs journeyId False
        Just endLegOrder -> do
          remainingLegs <- getRemainingLegs journeyId
          let legsToCancel = filter (\leg -> leg.order < endLegOrder) remainingLegs
          -- checkIfRemainingLegsAreCancellable legsToCancel
          mapM_
            ( \leg -> do
                isCancellable <- checkIfCancellable leg
                if isCancellable
                  then cancelLeg leg (SCR.CancellationReasonCode "") False False
                  else QJourneyLeg.updateIsDeleted (Just True) (Just leg.searchId)
            )
            legsToCancel

    getNewOriginLatLon legExtraInfo =
      case legExtraInfo of
        JL.Walk info -> return (info.origin.lat, info.origin.lon)
        JL.Taxi info -> return (info.origin.lat, info.origin.lon)
        JL.Metro info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> getLatLon "Metro" firstRoute.originStop.lat firstRoute.originStop.lon
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Metro leg info"
        JL.Subway info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> getLatLon "Subway" firstRoute.originStop.lat firstRoute.originStop.lon
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Subway leg info"
        JL.Bus info -> getLatLon "Bus" info.originStop.lat info.originStop.lon

    getLatLon label mLat mLon = do
      lat <- fromMaybeM (InvalidRequest $ label <> " latitude not found") mLat
      lon <- fromMaybeM (InvalidRequest $ label <> " longitude not found") mLon
      return (lat, lon)

    mkMultiModalLeg distance duration mode originLat originLon destLat destLon startTime = do
      now <- getCurrentTime
      let newStartTime = max now startTime
      return $
        MultiModalTypes.MultiModalLeg
          { distance,
            duration,
            polyline = Polyline {encodedPolyline = ""},
            mode,
            startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
            endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
            fromStopDetails = Nothing,
            toStopDetails = Nothing,
            routeDetails = [],
            serviceTypes = [],
            agency = Nothing,
            fromArrivalTime = Just newStartTime,
            fromDepartureTime = Just newStartTime,
            toArrivalTime = Nothing,
            toDepartureTime = Nothing,
            entrance = Nothing,
            exit = Nothing
          }

    mkExtendLegKey = "Extend:Leg:For:JourneyId-" <> journeyId.getId

    mkLocationAddress DLocation.LocationAPIEntity {..} = LA.LocationAddress {..}

    mkAddressFromStation name =
      LA.LocationAddress
        { street = Nothing,
          door = Nothing,
          city = Nothing,
          state = Nothing,
          country = Nothing,
          building = Nothing,
          areaCode = Nothing,
          area = Just name,
          ward = Nothing,
          placeId = Nothing,
          instructions = Nothing,
          title = Just name,
          extras = Nothing
        }

checkIfRemainingLegsAreCancellable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  [JL.LegInfo] ->
  m ()
checkIfRemainingLegsAreCancellable legsToCancel = do
  forM_ legsToCancel $ \leg -> do
    isCancellable <- checkIfCancellable leg
    unless isCancellable $
      throwError $ InvalidRequest $ "Cannot cancel leg for leg order: " <> show leg.order

checkIfAllLegsCancellable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  [JL.LegInfo] ->
  m Bool
checkIfAllLegsCancellable remainingLegs = do
  cancellableResults <- mapM checkIfCancellable remainingLegs
  return (and cancellableResults)

getExtendLegs ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  Id DJourney.Journey ->
  Int ->
  m [JL.LegInfo]
getExtendLegs journeyId legOrder = do
  journeyLegs <- getAllLegsInfo journeyId False
  let remainingLegs = filter (\leg -> notElem (leg.status) JL.cannotCancelWalkStatus && leg.order <= legOrder) journeyLegs
  return remainingLegs

extendLegEstimatedFare ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  Id DJourney.Journey ->
  JL.ExtendLegStartPoint ->
  Maybe DLocation.LocationAPIEntity ->
  Maybe Int ->
  m APITypes.ExtendLegGetFareResp
extendLegEstimatedFare journeyId startPoint mbEndLocation legOrder = do
  journey <- getJourney journeyId
  allLegs <- getAllLegsInfo journeyId False
  remainingLegs <- case legOrder of
    Just order -> getExtendLegs journeyId order
    Nothing -> getRemainingLegs journeyId

  currentLeg <- case startPoint of
    JL.StartLegOrder startLegOrder -> find (\leg -> leg.order == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)
    JL.StartLocation startLocation -> find (\leg -> leg.order == startLocation.legOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLocation.legOrder)

  -- isLegsCancellable <- checkIfAllLegsCancellable remainingLegs
  -- if isLegsCancellable
  --   then do
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (SearchRequestNotFound journey.searchRequestId.getId)
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for searchId: " <> show parentSearchReq.id.getId) parentSearchReq.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation

  startLocation <- getStartLocation startPoint remainingLegs
  case (startPoint, currentLeg.travelMode) of
    (JL.StartLocation _, DTrip.Taxi) -> do
      booking <- QBooking.findByTransactionId currentLeg.searchId >>= fromMaybeM (BookingNotFound $ "transactionId:-" <> currentLeg.searchId)
      ride <- QRide.findByRBId booking.id >>= fromMaybeM (InvalidRequest $ "No Ride present for booking" <> booking.id.getId)
      let editLocReq =
            DRide.EditLocationReq
              { origin = Nothing,
                destination = Just $ DRide.EditLocation {gps = LatLong {lat = endLocation.lat, lon = endLocation.lon}, address = getAddress endLocation}
              }
      editLocResp <- DRide.editLocation ride.id (currentLeg.personId, currentLeg.merchantId) editLocReq -- handle case if driver declines
      case editLocResp.bookingUpdateRequestId of
        Just bookingUpdateReqId -> do
          searchForUpdateRequest bookingUpdateReqId (5 :: Int) -- can set in config
        Nothing -> throwError (InvalidRequest "bookingUpdateRequestId not found")
    (_, _) -> do
      distResp <-
        Maps.getDistance currentLeg.merchantId currentLeg.merchantOperatingCityId (Just journeyId.getId) $
          Maps.GetDistanceReq
            { origin = startLocation,
              destination = getEndLocation endLocation,
              travelMode = Just Maps.CAR,
              sourceDestinationMapping = Nothing,
              distanceUnit = Meter
            }
      let distance = convertMetersToDistance Meter distResp.distance
      now <- getCurrentTime
      let multiModalLeg = mkMultiModalLeg distance distResp.duration MultiModalTypes.Unspecified startLocation.lat startLocation.lon endLocation.lat endLocation.lon
      estimatedFare <- JLI.getFare (Just now) journey.riderId currentLeg.merchantId currentLeg.merchantOperatingCityId multiModalLeg DTrip.Taxi
      return $
        APITypes.ExtendLegGetFareResp
          { totalFare = estimatedFare,
            distance = distance,
            duration = Just distResp.duration,
            bookingUpdateRequestId = Nothing
          }
  where
    getEndLocation location =
      LatLong
        { lat = location.lat,
          lon = location.lon
        }
    getStartLocation (JL.StartLocation startloc) _ = pure $ LatLong {lat = startloc.location.lat, lon = startloc.location.lon}
    getStartLocation (JL.StartLegOrder startLegOrder) legs = do
      startLeg <- find (\leg -> leg.order == startLegOrder) legs & fromMaybeM (InvalidRequest ("Journey Leg not Present" <> show startLegOrder))
      case startLeg.legExtraInfo of
        JL.Taxi info -> mkLatLng info.origin
        JL.Walk info -> mkLatLng info.origin
        JL.Metro info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> mkLatLngFromFRFS firstRoute.originStop -- Fetch the stop where subOrder is 1
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Metro info"
        JL.Bus info -> mkLatLngFromFRFS info.originStop
        JL.Subway info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> mkLatLngFromFRFS firstRoute.originStop -- Fetch the stop where subOrder is 1
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Subway info"

    mkLatLng originLocation = pure $ LatLong {lat = originLocation.lat, lon = originLocation.lon}
    mkLatLngFromFRFS startLoc = do
      latitude <- startLoc.lat & fromMaybeM (InvalidRequest "Start location not Found")
      longitude <- startLoc.lon & fromMaybeM (InvalidRequest "Start location not Found")
      return $ LatLong {lat = latitude, lon = longitude}

    searchForUpdateRequest bookingUpdateReqId 0 = throwError (InvalidRequest $ "Maximum number of tries reached for editLocation results for bookingUpdateReqId: " <> show bookingUpdateReqId)
    searchForUpdateRequest bookingUpdateReqId count = do
      bookingUpdateReq <- QBUR.findById bookingUpdateReqId >>= fromMaybeM (InvalidRequest $ "bookingUpdateRequest not found for id: " <> show bookingUpdateReqId)
      case bookingUpdateReq.estimatedFare of
        Nothing -> do
          liftIO $ threadDelaySec $ Seconds {getSeconds = 2} -- can set in config
          searchForUpdateRequest bookingUpdateReqId (count -1)
        Just estimatedFare -> do
          estimatedDistance <- bookingUpdateReq.estimatedDistance & fromMaybeM (InvalidRequest $ "EditLocation distance not Found for bookingUpdateReqId: " <> show bookingUpdateReq.id)
          return $
            APITypes.ExtendLegGetFareResp
              { totalFare = Just JL.GetFareResponse {estimatedMinFare = estimatedFare, estimatedMaxFare = estimatedFare, serviceTypes = Nothing},
                distance = convertHighPrecMetersToDistance bookingUpdateReq.distanceUnit estimatedDistance,
                duration = Nothing,
                bookingUpdateRequestId = Just bookingUpdateReq.id
              }

    mkMultiModalLeg distance duration mode originLat originLon destLat destLon =
      MultiModalTypes.MultiModalLeg
        { distance,
          duration,
          polyline = Polyline {encodedPolyline = ""},
          mode,
          startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
          endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
          fromStopDetails = Nothing,
          toStopDetails = Nothing,
          routeDetails = [],
          serviceTypes = [],
          agency = Nothing,
          fromArrivalTime = Nothing,
          fromDepartureTime = Nothing,
          toArrivalTime = Nothing,
          toDepartureTime = Nothing,
          entrance = Nothing,
          exit = Nothing
        }

    getAddress DLocation.LocationAPIEntity {..} = LA.LocationAddress {..}

switchLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  Id DJourney.Journey ->
  APITypes.SwitchLegReq ->
  m ()
switchLeg journeyId req = do
  journeyLegs <- getJourneyLegs journeyId
  remainingLegs <- getRemainingLegs journeyId
  journeyLeg <- find (\leg -> leg.sequenceNumber == req.legOrder) journeyLegs & fromMaybeM (InvalidRequest ("Journey Leg not Present" <> show req.legOrder))
  legData <- find (\leg -> leg.order == req.legOrder) remainingLegs & fromMaybeM (InvalidRequest ("Journey Leg not Present" <> show req.legOrder))
  startLocation <- return $ fromMaybe journeyLeg.startLocation req.startLocation
  (newDistance, newDuration) <-
    case req.newMode of
      DTrip.Walk -> do
        merchantId <- journeyLeg.merchantId & fromMaybeM (InvalidRequest $ "MerchantId not found for journeyLegId: " <> journeyLeg.id.getId)
        merchantOperatingCityId <- journeyLeg.merchantOperatingCityId & fromMaybeM (InvalidRequest $ "MerchantOperatingCityId not found for journeyLegId: " <> journeyLeg.id.getId)
        newDistanceAndDuration <-
          Maps.getMultimodalWalkDistance merchantId merchantOperatingCityId (Just journeyLeg.id.getId) $
            Maps.GetDistanceReq
              { origin = LatLong {lat = startLocation.latitude, lon = startLocation.longitude},
                destination = LatLong {lat = journeyLeg.endLocation.latitude, lon = journeyLeg.endLocation.longitude},
                travelMode = Just Maps.FOOT,
                sourceDestinationMapping = Nothing,
                distanceUnit = Meter
              }
        return (Just newDistanceAndDuration.distanceWithUnit, Just newDistanceAndDuration.duration)
      _ -> return (journeyLeg.distance, journeyLeg.duration)
  canSwitch <- canBeSwitched legData req.newMode newDistance
  isCancellable <- checkIfCancellable legData
  unless isCancellable $ do throwError (JourneyLegCannotBeCancelled journeyLeg.id.getId)
  unless canSwitch $ do throwError (JourneyLegCannotBeSwitched journeyLeg.id.getId)
  let lockKey = multimodalLegSearchIdAccessLockKey journeyId.getId
  Redis.whenWithLockRedis lockKey 5 $ do
    cancelLeg legData (SCR.CancellationReasonCode "") False False
    newJourneyLeg <- createJourneyLegFromCancelledLeg journeyLeg req.newMode startLocation newDistance newDuration
    addAllLegs journeyId [newJourneyLeg]
    when (legData.status /= JL.InPlan) $
      fork "Start journey thread" $ withShortRetry $ startJourney [] Nothing journeyId

upsertJourneyLeg :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (DJourneyLeg.JourneyLeg -> m ())
upsertJourneyLeg journeyLeg = do
  (findOneWithKV [Se.And [Se.Is BJourneyLeg.id $ Se.Eq (Kernel.Types.Id.getId journeyLeg.id)]]) >>= \case
    Just _ -> QJourneyLeg.updateByPrimaryKey journeyLeg
    Nothing -> QJourneyLeg.create journeyLeg

mkJourneyChangeLogKey :: Text -> Text
mkJourneyChangeLogKey journeyId = "JCCounter:JId-" <> journeyId

updateJourneyChangeLogCounter :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m ()
updateJourneyChangeLogCounter journeyId = do
  mbJourneyChangeLogCounter :: Maybe Int <- Redis.safeGet (mkJourneyChangeLogKey journeyId.getId)
  Redis.setExp (mkJourneyChangeLogKey journeyId.getId) (maybe 1 (+ 1) mbJourneyChangeLogCounter) 14400 -- 4 hours

getJourneyChangeLogCounter :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m Int
getJourneyChangeLogCounter journeyId = do
  mbJourneyChangeLogCounter <- Redis.safeGet (mkJourneyChangeLogKey journeyId.getId)
  return $ fromMaybe 0 mbJourneyChangeLogCounter

generateJourneyInfoResponse :: (CacheFlow m r, EsqDBFlow m r) => DJourney.Journey -> [JL.LegInfo] -> m APITypes.JourneyInfoResp
generateJourneyInfoResponse journey legs = do
  let estimatedMinFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMinFare <&> (.amount)) legs
  let estimatedMaxFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMaxFare <&> (.amount)) legs
  let unifiedQR = getUnifiedQR journey legs
  let mbCurrency = listToMaybe legs >>= (\leg -> leg.estimatedMinFare <&> (.currency))
  merchantOperatingCity <- maybe (pure Nothing) QMerchOpCity.findById journey.merchantOperatingCityId
  let merchantOperatingCityName = show . (.city) <$> merchantOperatingCity
  let unifiedQRV2 = getUnifiedQRV2 unifiedQR
  pure $
    APITypes.JourneyInfoResp
      { estimatedDuration = journey.estimatedDuration,
        estimatedMinFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMinFareAmount,
        estimatedMaxFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMaxFareAmount,
        estimatedDistance = journey.estimatedDistance,
        journeyStatus = journey.status,
        legs,
        unifiedQR,
        journeyId = journey.id,
        startTime = journey.startTime,
        endTime = journey.endTime,
        merchantOperatingCityName,
        crisSdkToken = Nothing,
        paymentOrderShortId = journey.paymentOrderShortId,
        unifiedQRV2
      }
  where
    getUnifiedQRV2 :: Maybe JL.UnifiedTicketQR -> Maybe JL.UnifiedTicketQRV2
    getUnifiedQRV2 mbUnifiedQR =
      mbUnifiedQR <&> convertUnifiedQRToV2

    convertUnifiedQRToV2 :: JL.UnifiedTicketQR -> JL.UnifiedTicketQRV2
    convertUnifiedQRToV2 unifiedQR =
      JL.UnifiedTicketQRV2
        { version = unifiedQR.version,
          _type = unifiedQR._type,
          txnId = unifiedQR.txnId,
          createdAt = unifiedQR.createdAt,
          cmrl = map convertBookingDataToV2 unifiedQR.cmrl,
          mtc = map convertBookingDataToV2 unifiedQR.mtc
        }

    convertBookingDataToV2 :: JL.BookingData -> JL.BookingDataV2
    convertBookingDataToV2 booking =
      JL.BookingDataV2
        { bookingId = booking.bookingId,
          isRoundTrip = booking.isRoundTrip,
          ticketData = booking.ticketData
        }

generateJourneyStatusResponse ::
  Id DPerson.Person ->
  Id DMerchant.Merchant ->
  DJourney.Journey ->
  [JL.JourneyLegState] ->
  Flow APITypes.JourneyStatusResp
generateJourneyStatusResponse personId merchantId journey legs = do
  journeyChangeLogCounter <- getJourneyChangeLogCounter journey.id
  paymentStatus <-
    if journey.isPaymentSuccess /= Just True
      then do
        allJourneyFrfsBookings <- QTBooking.findAllByJourneyIdCond (Just journey.id)
        frfsBookingStatusArr <- mapM (FRFSTicketService.frfsBookingStatus (personId, merchantId) True) allJourneyFrfsBookings
        let anyFirstBooking = listToMaybe frfsBookingStatusArr
            paymentOrder =
              anyFirstBooking >>= (.payment)
                <&> ( \p ->
                        APITypes.PaymentOrder {sdkPayload = p.paymentOrder, status = p.status}
                    )
        return $ paymentOrder <&> (.status)
      else return (Just FRFSTicketService.SUCCESS)
  return $ APITypes.JourneyStatusResp {legs = concatMap transformLeg legs, journeyStatus = journey.status, journeyPaymentStatus = paymentStatus, journeyChangeLogCounter}
  where
    transformLeg :: JL.JourneyLegState -> [APITypes.LegStatus]
    transformLeg legState =
      case legState of
        JL.Single legData -> [convert legData]
        JL.Transit legDataList -> map convert legDataList
      where
        convert legData =
          APITypes.LegStatus
            { legOrder = legData.legOrder,
              subLegOrder = legData.subLegOrder,
              status = legData.status,
              userPosition = legData.userPosition,
              vehiclePositions = legData.vehiclePositions,
              mode = legData.mode
            }

markLegStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => JL.JourneyLegStatus -> JL.LegExtraInfo -> m ()
markLegStatus status journeyLegExtraInfo = do
  case journeyLegExtraInfo of
    JL.Taxi legExtraInfo -> whenJust legExtraInfo.bookingId $ QBooking.updateJourneyLegStatus (Just status)
    JL.Metro legExtraInfo -> whenJust legExtraInfo.bookingId $ QTBooking.updateJourneyLegStatus (Just status)
    JL.Bus legExtraInfo -> whenJust legExtraInfo.bookingId $ QTBooking.updateJourneyLegStatus (Just status)
    JL.Subway legExtraInfo -> whenJust legExtraInfo.bookingId $ QTBooking.updateJourneyLegStatus (Just status)
    JL.Walk legExtraInfo -> QWalkLeg.updateStatus (JL.castWalkLegStatusFromLegStatus status) legExtraInfo.id
