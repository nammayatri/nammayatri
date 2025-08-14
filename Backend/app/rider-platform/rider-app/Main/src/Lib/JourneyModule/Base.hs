module Lib.JourneyModule.Base where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Applicative ((<|>))
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
import qualified Domain.Types.BookingStatus as DTaxiBooking
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateStatus as DTaxiEstimate
import Domain.Types.Extra.Ride as DRide
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSTicketBooking as DFRFSBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.LocationAddress as LA
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RideStatus as DTaxiRide
import qualified Domain.Types.RiderConfig
import qualified Domain.Types.Trip as DTrip
import Environment
import EulerHS.Prelude (safeHead)
import Kernel.Beam.Functions
import Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface as KMultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
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
import qualified Lib.JourneyModule.State.Types as JMState
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import qualified Lib.JourneyModule.Types as JL
import Lib.JourneyModule.Utils
import Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.GateInfo as GD
import qualified Sequelize as Se
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.Search
import qualified Storage.Beam.JourneyLeg as BJourneyLeg
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchOpCity
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyExtra as QJourneyExtra
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.MultiModal as TMultiModal

filterTransitRoutes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => Domain.Types.RiderConfig.RiderConfig -> [MultiModalRoute] -> m [MultiModalRoute]
filterTransitRoutes riderConfig routes = do
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
  searchReq <- QSearchRequest.findById journeyReq.parentSearchId >>= fromMaybeM (SearchRequestNotFound journeyReq.parentSearchId.getId)
  let fromLocation = searchReq.fromLocation
  let toLocation = searchReq.toLocation
  let legsWithContext = zip3 (Nothing : map Just journeyReq.legs) journeyReq.legs (map Just (tail journeyReq.legs) ++ [Nothing])
  legsAndFares <-
    mapWithIndex
      ( \idx (mbPrev, leg, mbNext) -> do
          let travelMode = convertMultiModalModeToTripMode leg.mode (straightLineDistance leg) (distanceToMeters leg.distance) journeyReq.maximumWalkDistance journeyReq.straightLineThreshold
          legFare@(_, mbTotalLegFare) <- measureLatency (JLI.getFare leg.fromArrivalTime journeyReq.personId journeyReq.merchantId journeyReq.merchantOperatingCityId leg travelMode) "multimodal getFare"
          journeyLeg <- JL.mkJourneyLeg idx (mbPrev, leg, mbNext) fromLocation toLocation journeyReq.personId journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance journeyReq.straightLineThreshold mbTotalLegFare
          return (legFare, journeyLeg)
      )
      legsWithContext

  let journeyFareLegs@(mbTotalFares, journeyLegs) = unzip legsAndFares
  logDebug $ "[Multimodal - Legs] : Is Multimodal Testing => " <> show riderConfig.multimodalTesting <> ", " <> show journeyFareLegs
  if not riderConfig.multimodalTesting && (any (\(isFareMandatory, mbLegFare) -> isFareMandatory && isNothing mbLegFare) mbTotalFares)
    then do return Nothing
    else do
      forM_ journeyLegs $ \leg -> do
        QJourneyLeg.create leg
      hasUserPreferredTransitTypesFlag <- hasUserPreferredTransitTypes journeyLegs userPreferences
      hasUserPreferredTransitModesFlag <- hasUserPreferredTransitModes journeyLegs userPreferences
      journey <- JL.mkJourney journeyReq.personId journeyReq.startTime journeyReq.endTime journeyReq.estimatedDistance journeyReq.estimatedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId journeyReq.legs journeyReq.maximumWalkDistance journeyReq.straightLineThreshold (searchReq.recentLocationId) journeyReq.relevanceScore hasUserPreferredTransitTypesFlag hasUserPreferredTransitModesFlag fromLocation toLocation
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
getJourneyLegs = QJourneyLeg.getJourneyLegs

getAllLegsInfoWithoutAddingSkipLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getAllLegsInfoWithoutAddingSkipLeg journeyId = getAllLegsInfo journeyId

-- not needed as we are using getJourneyLegs from JourneyLegExtra and it is already
-- sorted by sequenceNumber and filtered out deleted legs by default from db
-- legs <- QJourneyLeg.findAllByJourneyId journeyId
-- let filteredLegs = filter (\leg -> leg.isDeleted == Just False || leg.isDeleted == Nothing) legs
-- return $ sortBy (comparing (.sequenceNumber)) filteredLegs

multiModalTravelModeToBecknVehicleCategory :: DTrip.MultimodalTravelMode -> Maybe BecknSpec.VehicleCategory
multiModalTravelModeToBecknVehicleCategory = \case
  DTrip.Metro -> Just BecknSpec.METRO
  DTrip.Bus -> Just BecknSpec.BUS
  DTrip.Subway -> Just BecknSpec.SUBWAY
  _ -> Nothing

getAllLegsInfo ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getAllLegsInfo journeyId = do
  whenJourneyUpdateInProgress journeyId $ do
    allLegsRawData <- getJourneyLegs journeyId
    mapMaybeM getLegInfo allLegsRawData

getLegInfo ::
  JL.GetStateFlow m r c =>
  DJourneyLeg.JourneyLeg ->
  m (Maybe JL.LegInfo)
getLegInfo journeyLeg = do
  case journeyLeg.legSearchId of
    Just legSearchIdText -> do
      let legSearchId = Id legSearchIdText
      case journeyLeg.mode of
        DTrip.Taxi -> JL.getInfo $ TaxiLegRequestGetInfo $ TaxiLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg}
        DTrip.Walk -> JL.getInfo $ WalkLegRequestGetInfo $ WalkLegRequestGetInfoData {journeyLeg = journeyLeg}
        DTrip.Metro -> JL.getInfo $ MetroLegRequestGetInfo $ MetroLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg = journeyLeg}
        DTrip.Subway -> JL.getInfo $ SubwayLegRequestGetInfo $ SubwayLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg = journeyLeg}
        DTrip.Bus -> JL.getInfo $ BusLegRequestGetInfo $ BusLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg = journeyLeg}
    Nothing -> return Nothing

hasSignificantMovement :: [LatLong] -> Domain.Types.RiderConfig.BusTrackingConfig -> Bool
hasSignificantMovement (p1 : p2 : _) busTrackingConfig =
  let d1 = highPrecMetersToMeters $ distanceBetweenInMeters p1 p2
   in d1 > (round busTrackingConfig.movementThresholdInMeters)
hasSignificantMovement _ _ = False

getRiderConfig :: (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) => DJourney.Journey -> m Domain.Types.RiderConfig.RiderConfig
getRiderConfig journey = QRiderConfig.findByMerchantOperatingCityId journey.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist journey.merchantOperatingCityId.getId)

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

checkAndMarkTerminalJourneyStatus ::
  (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  DJourney.Journey ->
  Bool ->
  Bool ->
  [JL.JourneyLegState] ->
  m ()
checkAndMarkTerminalJourneyStatus journey feedbackRequired isCancelSearchApi = go . concatLegStates
  where
    concatLegStates =
      foldl'
        ( \acc st -> case st of
            JL.Single legState -> [legState] <> acc
            JL.Transit legStates -> legStates <> acc
        )
        []

    isCancelled :: JL.JourneyLegStateData -> Bool
    isCancelled legState =
      legState.status == JL.Cancelled -- TODO: Remove this once below is used always
        || (if legState.mode == DTrip.Walk then legState.trackingStatus == Just JMState.Finished else maybe False (\status -> status `elem` [JMState.TaxiEstimate DTaxiEstimate.CANCELLED, JMState.TaxiBooking DTaxiBooking.CANCELLED, JMState.TaxiRide DTaxiRide.CANCELLED, JMState.FRFSBooking DFRFSBooking.CANCELLED, JMState.FRFSTicket DFRFSTicket.CANCELLED]) legState.bookingStatus)

    isCompleted :: JL.JourneyLegStateData -> Bool
    isCompleted legState =
      legState.status `elem` journeyLegTerminalStatuses -- TODO: Remove this once below is used always
        || (if legState.mode == DTrip.Walk then legState.trackingStatus == Just JMState.Finished else maybe False (\status -> status `elem` [JMState.TaxiBooking DTaxiBooking.COMPLETED, JMState.TaxiRide DTaxiRide.COMPLETED, JMState.FRFSTicket DFRFSTicket.USED] || (not feedbackRequired && legState.bookingStatus == Just (JMState.Feedback JMState.FEEDBACK_PENDING))) legState.bookingStatus)

    isFeedbackPending :: JL.JourneyLegStateData -> Bool
    isFeedbackPending legState =
      legState.status `elem` journeyLegTerminalStatuses -- TODO: Remove this once below is used always
        || ( feedbackRequired
               && (if legState.mode == DTrip.Walk then legState.trackingStatus == Just JMState.Finished else legState.bookingStatus == Just (JMState.Feedback JMState.FEEDBACK_PENDING))
           )

    go allLegsState
      | all isCancelled allLegsState =
        updateJourneyStatus journey DJourney.CANCELLED
      | all isCompleted allLegsState && (journey.status == DJourney.FEEDBACK_PENDING || not feedbackRequired) =
        updateJourneyStatus journey DJourney.COMPLETED
      | all isFeedbackPending allLegsState =
        updateJourneyStatus journey DJourney.FEEDBACK_PENDING
      | otherwise = pure ()

    journeyLegTerminalStatuses = if isCancelSearchApi then [JL.Completed, JL.Cancelled, JL.Skipped] else [JL.Completed, JL.Cancelled]

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
  let legsWithNext = zip allLegsRawData $ map Just (tail allLegsRawData) ++ [Nothing]
  logDebug $ "getAllLegsStatus: legsWithNext: " <> show legsWithNext
  (_, legPairs) <- foldlM (processLeg riderLastPoints allLegsRawData movementDetected) (Nothing, []) legsWithNext
  let allLegsState = map snd legPairs
  -- Update journey expiry time to the next valid ticket expiry when a leg is completed
  whenJust (minimumTicketLegOrder legPairs) $ \nextLegOrder -> do
    QJourneyExtra.updateJourneyToNextTicketExpiryTime journey.id nextLegOrder
  checkAndMarkTerminalJourneyStatus journey True False allLegsState
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
    getRouteCodeToTrack leg = safeHead leg.routeDetails >>= ((.routeGtfsId) >=> (pure . gtfsIdtoDomainCode))

    processLeg ::
      (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
      [APITypes.RiderLocationReq] ->
      [DJourneyLeg.JourneyLeg] ->
      Bool ->
      (Maybe DJourneyLeg.JourneyLeg, [(DJourneyLeg.JourneyLeg, JL.JourneyLegState)]) ->
      (DJourneyLeg.JourneyLeg, Maybe DJourneyLeg.JourneyLeg) ->
      m (Maybe DJourneyLeg.JourneyLeg, [(DJourneyLeg.JourneyLeg, JL.JourneyLegState)])
    processLeg riderLastPoints allLegsRawData movementDetected (lastLeg, legsState) (leg, mbNextLeg) = do
      case leg.legSearchId of
        Just legSearchIdText -> do
          let legSearchId = Id legSearchIdText
          legState <-
            case leg.mode of
              DTrip.Taxi -> JL.getState $ TaxiLegRequestGetState $ TaxiLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, journeyLeg = leg}
              DTrip.Walk -> JL.getState $ WalkLegRequestGetState $ WalkLegRequestGetStateData {riderLastPoints, journeyLeg = leg}
              DTrip.Metro -> JL.getState $ MetroLegRequestGetState $ MetroLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, journeyLeg = leg}
              DTrip.Subway -> JL.getState $ SubwayLegRequestGetState $ SubwayLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, journeyLeg = leg}
              DTrip.Bus -> do
                logDebug $ "BusLegRequestGetStateData: " <> show legSearchId <> ", " <> show leg
                JL.getState $ BusLegRequestGetState $ BusLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, movementDetected, routeCodeForDetailedTracking = getRouteCodeToTrack leg, journeyLeg = leg}
          return
            ( Just leg,
              legsState <> [(leg, legState)]
            )
        Nothing -> do
          logError $ "LegId is null for JourneyLeg: " <> show leg.journeyId <> " JourneyLegId: " <> show leg.id
          addAllLegs journey.id (Just allLegsRawData) [leg] -- try to add the leg again
          updatedLeg <- QJourneyLeg.findByPrimaryKey leg.id >>= fromMaybeM (JourneyLegNotFound leg.id.getId)
          case updatedLeg.legSearchId of
            Just _ -> do
              processLeg riderLastPoints allLegsRawData movementDetected (lastLeg, legsState) (updatedLeg, mbNextLeg)
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
  allLegs <- getAllLegsInfo journeyId
  mapM_ (\leg -> QTBooking.updateOnInitDoneBySearchId (Just False) (Id leg.searchId)) allLegs -- TODO :: Handle the case where isMultiAllowed is False
  mapM_
    ( \leg -> do
        let mElement = find (\element -> element.journeyLegOrder == leg.order) confirmElements
            ticketQuantity = mElement >>= (.ticketQuantity)
            childTicketQuantity = mElement >>= (.childTicketQuantity)
        let forcedBooking = Just leg.order == forcedBookedLegOrder
        let crisSdkResponse = find (\element -> element.journeyLegOrder == leg.order) confirmElements >>= (.crisSdkResponse)
        when (leg.status == JL.InPlan) $ do
          JLI.confirm forcedBooking ticketQuantity childTicketQuantity leg crisSdkResponse
    )
    allLegs

addAllLegs ::
  ( JL.SearchRequestFlow m r c,
    JL.GetStateFlow m r c,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  Id DJourney.Journey ->
  Maybe [DJourneyLeg.JourneyLeg] ->
  [DJourneyLeg.JourneyLeg] ->
  m ()
addAllLegs journeyId mbOldJourneyLegs newJourneyLegs = do
  journey <- getJourney journeyId
  oldLegs <- maybe (getJourneyLegs journeyId) (\oldJourneyLegs -> return oldJourneyLegs) mbOldJourneyLegs
  let filteredOldLegs = filter (\leg1 -> all (\leg2 -> not (leg1.sequenceNumber == leg2.sequenceNumber)) newJourneyLegs) oldLegs
  let allLegs = sortBy (comparing (.sequenceNumber)) (filteredOldLegs ++ newJourneyLegs)
  toLocation <- journey.toLocation & fromMaybeM (InvalidRequest "To location nothing for Journey / Parent Search Request")
  forM_ (traverseWithTriplets allLegs) $ \(mbPrevJourneyLeg, journeyLeg, mbNextJourneyLeg) -> do
    when (isNothing journeyLeg.legSearchId) $ do
      -- In case of retry of this function, if search has already triggered then it will not do it again
      searchResp <-
        case journeyLeg.mode of
          DTrip.Taxi -> do
            snappedLeg <- snapJourneyLegToNearestGate journeyLeg
            let originAddress = mkAddress (mbPrevJourneyLeg >>= (.toStopDetails)) journeyLeg.mode journey.fromLocation.address
            let destinationAddress = mkAddress (mbNextJourneyLeg >>= (.fromStopDetails)) journeyLeg.mode toLocation.address
            addTaxiLeg journey snappedLeg originAddress destinationAddress
          DTrip.Metro -> do
            addMetroLeg journey journeyLeg
          DTrip.Subway -> do
            addSubwayLeg journey journeyLeg
          DTrip.Walk -> return $ JL.SearchResponse {id = journeyLeg.id.getId}
          DTrip.Bus -> do
            addBusLeg journey journeyLeg
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

    mkAddress :: Maybe MultiModalStopDetails -> DTrip.MultimodalTravelMode -> LA.LocationAddress -> LA.LocationAddress
    mkAddress Nothing _ parentAddress = parentAddress
    mkAddress (Just stopDetails) mode _ =
      let modeText = case mode of
            DTrip.Metro -> Just "Metro Station"
            DTrip.Bus -> Just "Bus Stop"
            _ -> Nothing
       in LA.LocationAddress
            { street = Nothing,
              door = Nothing,
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
              extras = modeText
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
          let destinations = LatLong (destLocation.latitude) (destLocation.longitude) :| []
          let originLocs = fmap (.point) nonEmptyGates
          distanceResponses <-
            Maps.getMultimodalJourneyDistances journeyLeg.merchantId journeyLeg.merchantOperatingCityId (Just journeyLeg.id.getId) $
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

mkLocationWithGate ::
  Maybe MultiModalLegGate ->
  LA.LocationAddress ->
  LatLngV2 ->
  SearchReqLocation
mkLocationWithGate mGate baseAddr fallbackLoc =
  case mGate of
    Just gate ->
      let lat = fromMaybe fallbackLoc.latitude gate.lat
          lon = fromMaybe fallbackLoc.longitude gate.lon
       in JL.mkSearchReqLocation
            (if isJust (gate.streetName) then baseAddr {LA.street = gate.streetName} else baseAddr)
            (LatLngV2 lat lon)
    Nothing -> JL.mkSearchReqLocation baseAddr fallbackLoc

addTaxiLeg ::
  JL.SearchRequestFlow m r c =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  LA.LocationAddress ->
  LA.LocationAddress ->
  m JL.SearchResponse
addTaxiLeg journey journeyLeg originAddress destinationAddress = do
  let startLocation = mkLocationWithGate journeyLeg.osmExit originAddress journeyLeg.startLocation
  let endLocation = mkLocationWithGate journeyLeg.osmEntrance destinationAddress journeyLeg.endLocation
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

addMetroLeg ::
  (JL.SearchRequestFlow m r c, JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addMetroLeg journey journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById journey.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound journey.merchantOperatingCityId.getId)
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
  let metroSearchReq = mkMetroLegReq merchantOperatingCity.city
  searchResp <- JL.search metroSearchReq

  now <- getCurrentTime
  shouldSkip <- case (riderConfig.qrTicketRestrictionStartTime, riderConfig.qrTicketRestrictionEndTime) of
    (Just startTime, Just endTime) -> do
      let isOutsideRestrictedHours = not $ isWithinTimeBound startTime endTime now riderConfig.timeDiffFromUtc
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
            personId = journey.riderId,
            merchantId = journey.merchantId,
            recentLocationId = journey.recentLocationId,
            city,
            journeyLeg
          }

addSubwayLeg ::
  JL.SearchRequestFlow m r c =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addSubwayLeg journey journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById journey.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound journey.merchantOperatingCityId.getId)
  let subwaySearchReq = mkSubwayLegReq merchantOperatingCity.city
  JL.search subwaySearchReq
  where
    mkSubwayLegReq city = do
      SubwayLegRequestSearch $
        SubwayLegRequestSearchData
          { quantity = 1,
            personId = journey.riderId,
            merchantId = journey.merchantId,
            recentLocationId = journey.recentLocationId,
            city,
            journeyLeg
          }

addBusLeg ::
  JL.SearchRequestFlow m r c =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addBusLeg journey journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById journey.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound journey.merchantOperatingCityId.getId)
  let busSearchReq = mkBusLegReq merchantOperatingCity.city
  JL.search busSearchReq
  where
    mkBusLegReq city = do
      BusLegRequestSearch $
        BusLegRequestSearchData
          { quantity = 1,
            personId = journey.riderId,
            merchantId = journey.merchantId,
            recentLocationId = journey.recentLocationId,
            city,
            journeyLeg
          }

isWithinTimeBound :: Time.TimeOfDay -> Time.TimeOfDay -> UTCTime -> Seconds -> Bool
isWithinTimeBound startTime endTime now timeDiffFromUtc =
  let tzMinutes = getSeconds timeDiffFromUtc `div` 60
      tz = Time.minutesToTimeZone tzMinutes
      nowAsLocal = Time.utcToLocalTime tz now
      nowTOD = Time.localTimeOfDay nowAsLocal

      --handle midnight wrap
      inWindow =
        if startTime <= endTime
          then nowTOD >= startTime && nowTOD <= endTime
          else nowTOD >= startTime || nowTOD <= endTime
   in inWindow

getRemainingLegs ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getRemainingLegs journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  let remainingLegs = filter cancellableStatus journeyLegs -- check if edge case is to be handled [completed , skipped, inplan]
  return remainingLegs

getRemainingLegsForExtend ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getRemainingLegsForExtend journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  let remainingLegs = filter cancellableExtendStatus journeyLegs
  return remainingLegs

cancellableExtendStatus :: JL.LegInfo -> Bool
cancellableExtendStatus leg =
  case leg.legExtraInfo of
    JL.Walk extraInfo -> maybe True (\status -> not (status `elem` JL.cannotCancelWalkStatus)) extraInfo.trackingStatus
    JL.Taxi extraInfo -> maybe True (\status -> not (status `elem` JL.cannotCancelExtendStatus)) extraInfo.trackingStatus
    JL.Bus extraInfo -> maybe True (not . (`elem` JL.cannotCancelExtendStatus)) extraInfo.trackingStatus
    JL.Subway extraInfo -> all (maybe True (not . (`elem` JL.cannotCancelExtendStatus)) . (.trackingStatus)) extraInfo.routeInfo
    JL.Metro extraInfo -> all (maybe True (not . (`elem` JL.cannotCancelExtendStatus)) . (.trackingStatus)) extraInfo.routeInfo

cancellableStatus :: JL.LegInfo -> Bool
cancellableStatus leg =
  case leg.legExtraInfo of
    JL.Walk extraInfo -> maybe True (\status -> not (status `elem` JL.cannotCancelWalkStatus)) extraInfo.trackingStatus
    JL.Taxi extraInfo -> maybe True (\status -> not (status `elem` JL.cannotCancelStatus)) extraInfo.trackingStatus
    JL.Bus extraInfo -> maybe True (not . (`elem` JL.cannotCancelStatus)) extraInfo.trackingStatus
    JL.Subway extraInfo -> all (maybe True (not . (`elem` JL.cannotCancelStatus)) . (.trackingStatus)) extraInfo.routeInfo
    JL.Metro extraInfo -> all (maybe True (not . (`elem` JL.cannotCancelStatus)) . (.trackingStatus)) extraInfo.routeInfo

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
  Id DJourney.Journey ->
  JL.LegInfo ->
  SCR.CancellationReasonCode ->
  Bool ->
  Bool ->
  Bool ->
  Maybe (Id DEstimate.Estimate) ->
  m ()
cancelLeg journeyId journeyLeg cancellationReasonCode isSkipped skippedDuringConfirmation shouldUpdateJourneyStatus cancelEstimateId = do
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
              cancelEstimateId,
              isSkipped
            }
    DTrip.Walk ->
      JL.cancel $
        WalkLegRequestCancel
          WalkLegRequestCancelData
            { journeyLegId = journeyLeg.journeyLegId
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
  when shouldUpdateJourneyStatus $ do
    journey <- getJourney journeyId
    updatedLegStatus <- getAllLegsStatus journey
    logError $ "Checking and marking terminal journey status for journey: " <> show journey.id.getId <> " with updatedLegStatus: " <> show (length updatedLegStatus)
    when (length updatedLegStatus == 1) $ do
      checkAndMarkTerminalJourneyStatus journey (not isSkipped) (isJust cancelEstimateId) updatedLegStatus
  -- whenJust journeyLeg.journeyLegId $ \journeyLegId -> do
  --   when isSkipped $ do
  --     JMStateUtils.setJourneyLegTrackingStatus journeyLegId Nothing JMState.Finished
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
            if isCancellable then cancelLeg journeyId leg (SCR.CancellationReasonCode "") False False False Nothing else QJourneyLeg.updateIsDeleted (Just True) (Just leg.searchId)
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
  allLegs <- getAllLegsInfo journeyId
  skippingLeg <- fromMaybeM (InvalidRequest $ "Leg not found: " <> show legOrder) $ find (\leg -> leg.order == legOrder) allLegs
  if skippingLeg.skipBooking
    then return ()
    else do
      when (skippingLeg.travelMode == DTrip.Walk) $
        throwError $ JourneyLegCannotBeSkippedForMode (show skippingLeg.travelMode)
      unless (cancellableStatus skippingLeg) $
        throwError $ JourneyLegCannotBeSkippedForStatus (show skippingLeg.status)
      cancelLeg journeyId skippingLeg (SCR.CancellationReasonCode "") True skippedDuringConfirmation True Nothing
  journey <- getJourney journeyId
  updatedLegStatus <- getAllLegsStatus journey
  if legOrder == length updatedLegStatus - 1
    then checkAndMarkTerminalJourneyStatus journey False False updatedLegStatus
    else pure ()

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

  exep <- try @_ @SomeException $ do
    QJourneyLeg.updateIsSkipped (Just False) skippedLeg.legSearchId
    addAllLegs journeyId (Just allLegs) [skippedLeg {DJourneyLeg.isSkipped = Just False, DJourneyLeg.legSearchId = Nothing}]
  case exep of
    Left _ -> do
      -- Rollback operations
      QJourneyLeg.updateLegSearchId skippedLeg.legSearchId skippedLeg.id
      QJourneyLeg.updateIsSkipped (Just True) skippedLeg.legSearchId
      throwError $ InvalidRequest "Failed to update skipped leg, as Search operation failed"
    Right _ -> return ()

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
            { legInfo = journeyLeg
            }
    DTrip.Metro ->
      JL.isCancellable $
        MetroLegRequestIsCancellable
          MetroLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId,
              legInfo = journeyLeg
            }
    DTrip.Subway -> JL.isCancellable $ SubwayLegRequestIsCancellable SubwayLegRequestIsCancellableData
    DTrip.Bus ->
      JL.isCancellable $
        BusLegRequestIsCancellable
          BusLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId,
              legInfo = journeyLeg
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
canBeSwitched legToBeSwitched newMode _ = do
  let currentMode = legToBeSwitched.travelMode
  case (currentMode, newMode) of
    (_, DTrip.Metro) -> return False
    (_, DTrip.Bus) -> return False
    (DTrip.Bus, DTrip.Taxi) -> return False
    (DTrip.Metro, DTrip.Taxi) -> return False
    (DTrip.Walk, DTrip.Taxi) -> return True
    (DTrip.Taxi, DTrip.Walk) -> return True
    -- commenting this so that UI can handle the distance check
    -- case newDistance of
    --   Just distance ->
    --     if getMeters (distanceToMeters distance) <= 2000
    --       then do return True
    --       else do throwError $ InvalidRequest "Can't switch to walk if distance is more than 2km, skip the ride instead"
    --   Nothing -> return True
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
        routeGroupId = journeyLeg.routeGroupId,
        riderId = journeyLeg.riderId,
        fromArrivalTime = Nothing,
        fromDepartureTime = Nothing,
        fromStopDetails = Nothing,
        id = journeyLegId,
        journeyId = journeyLeg.journeyId,
        mode = newMode,
        routeDetails = journeyLeg.routeDetails,
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
        osmEntrance = journeyLeg.osmEntrance,
        osmExit = journeyLeg.osmExit,
        straightLineEntrance = journeyLeg.straightLineEntrance,
        straightLineExit = journeyLeg.straightLineExit
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
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for journeyId: " <> show journey.id.getId) journey.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation
  allLegs <- getAllLegsInfo journeyId
  case startPoint of
    JL.StartLegOrder startLegOrder -> do
      currentLeg <- find (\leg -> leg.order == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)
      legsToCancel <-
        case mbEndLegOrder of
          Just endLegOrder -> return $ filter (\leg -> leg.order >= startLegOrder && leg.order < endLegOrder) allLegs
          Nothing -> return $ filter (\leg -> leg.order >= startLegOrder) allLegs
      -- checkIfRemainingLegsAreCancellable legsToCancel
      (newOriginLat, newOriginLon) <- getNewOriginLatLon currentLeg.legExtraInfo
      leg <- mkMultiModalTaxiLeg newDistance newDuration MultiModalTypes.Unspecified newOriginLat newOriginLon endLocation.lat endLocation.lon currentLeg.startTime
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg startLegOrder (Nothing, leg, Nothing) journey.fromLocation journey.toLocation journey.riderId currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance riderConfig.straightLineThreshold (Just fare)
      startLocationAddress <-
        case currentLeg.legExtraInfo of
          JL.Walk walkLegExtraInfo -> return walkLegExtraInfo.origin.address
          JL.Taxi taxiLegExtraInfo -> return taxiLegExtraInfo.origin.address
          _ -> do
            frfsSearchReq <- QFRFSSearch.findById (Id currentLeg.searchId) >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> currentLeg.searchId)
            integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity frfsSearchReq
            fromStation <- OTPRest.getStationByGtfsIdAndStopCode frfsSearchReq.fromStationCode integratedBPPConfig >>= fromMaybeM (InvalidRequest $ "from station not found in extendLeg: " <> show frfsSearchReq.fromStationCode)
            return $ mkAddressFromStation fromStation.name
      withJourneyUpdateInProgress journeyId $ do
        forM_ legsToCancel $ \currLeg -> do
          isCancellable <- checkIfCancellable currLeg
          if isCancellable
            then cancelLeg journeyId currLeg (SCR.CancellationReasonCode "") False False False Nothing
            else QJourneyLeg.updateIsDeleted (Just True) (Just currLeg.searchId)
        QJourneyLeg.create journeyLeg
        updateJourneyChangeLogCounter journeyId
        searchResp <- addTaxiLeg journey journeyLeg startLocationAddress (mkLocationAddress endLocation)
        QJourneyLeg.updateLegSearchId (Just searchResp.id) journeyLeg.id
        when (currentLeg.status /= JL.InPlan) $
          fork "Start journey thread" $ withShortRetry $ startJourney [] Nothing journeyId
    JL.StartLocation startlocation -> do
      currentLeg <- find (\leg -> leg.order == startlocation.legOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startlocation.legOrder)
      case (currentLeg.travelMode, currentLeg.skipBooking) of
        (DTrip.Taxi, False) -> do
          bookingUpdateRequestId <- bookingUpdateReqId & fromMaybeM (InvalidRequest "bookingUpdateReqId not found")
          journeyLeg <- QJourneyLeg.findByLegSearchId (Just currentLeg.searchId) >>= fromMaybeM (InvalidRequest $ "JourneyLeg not found for searchId: " <> currentLeg.searchId)
          void $ DEditLocation.postEditResultConfirm (Just journey.riderId, journey.merchantId) bookingUpdateRequestId
          Redis.setExp mkExtendLegKey journeyLeg.id 300 --5 mins
        (DTrip.Taxi, True) -> extendWalkLeg journey startlocation endLocation currentLeg
        (DTrip.Walk, _) -> extendWalkLeg journey startlocation endLocation currentLeg
        _ -> do
          throwError $ InvalidRequest ("Cannot extend leg for mode: " <> show currentLeg.travelMode)
  where
    extendWalkLeg journey startlocation endLocation currentLeg = do
      now <- getCurrentTime
      leg <- mkMultiModalTaxiLeg newDistance newDuration MultiModalTypes.Unspecified startlocation.location.lat startlocation.location.lon endLocation.lat endLocation.lon now
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg currentLeg.order (Nothing, leg, Nothing) journey.fromLocation journey.toLocation journey.riderId currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance riderConfig.straightLineThreshold (Just fare)
      withJourneyUpdateInProgress journeyId $ do
        cancelRequiredLegs
        QJourneyLeg.create journeyLeg
        searchResp <- addTaxiLeg journey journeyLeg (mkLocationAddress startlocation.location) (mkLocationAddress endLocation)
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
                  then cancelLeg journeyId leg (SCR.CancellationReasonCode "") False False False Nothing
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

    mkMultiModalTaxiLeg distance duration mode originLat originLon destLat destLon startTime = do
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
            routeDetails =
              [ MultiModalTypes.MultiModalRouteDetails
                  { gtfsId = Nothing,
                    longName = Nothing,
                    shortName = Nothing,
                    alternateShortNames = [],
                    color = Nothing,
                    fromStopDetails = Nothing,
                    toStopDetails = Nothing,
                    startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
                    endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
                    subLegOrder = 1,
                    fromArrivalTime = Just newStartTime,
                    fromDepartureTime = Just newStartTime,
                    toArrivalTime = Nothing,
                    toDepartureTime = Nothing
                  }
              ],
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
  journeyLegs <- getAllLegsInfo journeyId
  let remainingLegs = filter (\leg -> isCannotCancelStatus leg && leg.order <= legOrder) journeyLegs
  return remainingLegs
  where
    isCannotCancelStatus leg =
      case leg.legExtraInfo of
        JL.Walk extraInfo -> maybe True (\status -> not (status `elem` JL.cannotCancelWalkStatus)) extraInfo.trackingStatus
        JL.Taxi extraInfo -> maybe True (\status -> not (status `elem` JL.cannotCancelStatus)) extraInfo.trackingStatus
        JL.Bus extraInfo -> maybe True (\status -> not (status `elem` JL.cannotCancelStatus)) extraInfo.trackingStatus
        JL.Subway extraInfo -> all (maybe True (not . (`elem` JL.cannotCancelStatus)) . (.trackingStatus)) extraInfo.routeInfo
        JL.Metro extraInfo -> all (maybe True (not . (`elem` JL.cannotCancelStatus)) . (.trackingStatus)) extraInfo.routeInfo

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
  allLegs <- getAllLegsInfo journeyId
  remainingLegs <- case legOrder of
    Just order -> getExtendLegs journeyId order
    Nothing -> getRemainingLegs journeyId

  currentLeg <- case startPoint of
    JL.StartLegOrder startLegOrder -> find (\leg -> leg.order == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)
    JL.StartLocation startLocation -> find (\leg -> leg.order == startLocation.legOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLocation.legOrder)

  -- isLegsCancellable <- checkIfAllLegsCancellable remainingLegs
  -- if isLegsCancellable
  --   then do
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for journeyId: " <> show journey.id.getId) journey.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation

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
      let multiModalLeg = mkMultiModalTaxiLeg distance distResp.duration MultiModalTypes.Unspecified startLocation.lat startLocation.lon endLocation.lat endLocation.lon
      (isFareMandatory, estimatedFare) <- JLI.getFare (Just now) journey.riderId currentLeg.merchantId currentLeg.merchantOperatingCityId multiModalLeg DTrip.Taxi
      when (isFareMandatory && isNothing estimatedFare) $ throwError (InvalidRequest "Fare is mandatory for this leg, but unavailable")
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

    mkMultiModalTaxiLeg distance duration mode originLat originLon destLat destLon =
      MultiModalTypes.MultiModalLeg
        { distance,
          duration,
          polyline = Polyline {encodedPolyline = ""},
          mode,
          startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
          endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
          fromStopDetails = Nothing,
          toStopDetails = Nothing,
          routeDetails =
            [ MultiModalTypes.MultiModalRouteDetails
                { gtfsId = Nothing,
                  longName = Nothing,
                  shortName = Nothing,
                  alternateShortNames = [],
                  color = Nothing,
                  fromStopDetails = Nothing,
                  toStopDetails = Nothing,
                  startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
                  endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
                  subLegOrder = 1,
                  fromArrivalTime = Nothing,
                  fromDepartureTime = Nothing,
                  toArrivalTime = Nothing,
                  toDepartureTime = Nothing
                }
            ],
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
        newDistanceAndDuration <-
          Maps.getMultimodalWalkDistance journeyLeg.merchantId journeyLeg.merchantOperatingCityId (Just journeyLeg.id.getId) $
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
    cancelLeg journeyId legData (SCR.CancellationReasonCode "") False False False Nothing
    newJourneyLeg <- createJourneyLegFromCancelledLeg journeyLeg req.newMode startLocation newDistance newDuration
    addAllLegs journeyId (Just journeyLegs) [newJourneyLeg]
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
  merchantOperatingCity <- QMerchOpCity.findById journey.merchantOperatingCityId
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
        paymentOrderShortId = journey.paymentOrderShortId,
        unifiedQRV2,
        result = Just "Success"
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
              bookingStatus = legData.bookingStatus,
              trackingStatus = legData.trackingStatus,
              userPosition = legData.userPosition,
              vehiclePositions = legData.vehiclePositions,
              mode = legData.mode
            }

markLegStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe JL.JourneyLegStatus -> Maybe JMState.TrackingStatus -> DJourneyLeg.JourneyLeg -> Maybe Int -> m ()
markLegStatus mbStatus trackingStatus journeyLeg mbSubLegOrder = do
  let finalStatus = trackingStatus <|> castJourneyLegStatusToTrackingStatus mbStatus
  whenJust finalStatus $ \status -> do
    JMStateUtils.setJourneyLegTrackingStatus journeyLeg mbSubLegOrder status
  where
    castJourneyLegStatusToTrackingStatus :: Maybe JL.JourneyLegStatus -> Maybe JMState.TrackingStatus
    castJourneyLegStatusToTrackingStatus = \case
      Just JL.InPlan -> Just JMState.InPlan
      Just JL.Assigning -> Just JMState.InPlan
      Just JL.Booked -> Just JMState.InPlan
      Just JL.AtRiskOfMissing -> Just JMState.InPlan
      Just JL.Missed -> Just JMState.InPlan
      Just JL.Delayed -> Just JMState.InPlan
      Just JL.OnTheWay -> Just JMState.Arriving
      Just JL.Arriving -> Just JMState.AlmostArrived
      Just JL.Arrived -> Just JMState.Arrived
      Just JL.Ongoing -> Just JMState.Ongoing
      Just JL.Finishing -> Just JMState.Finishing
      Just JL.Skipped -> Just JMState.Finished
      Just JL.Cancelled -> Just JMState.Finished
      Just JL.Completed -> Just JMState.Finished
      Just JL.Failed -> Just JMState.Finished
      Nothing -> Nothing
