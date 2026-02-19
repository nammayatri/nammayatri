module Lib.JourneyModule.Base where

import qualified API.Types.UI.FRFSTicketService as APITypes
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Monad.Extra (mapMaybeM)
import Domain.Action.UI.EditLocation as DEditLocation
import qualified Domain.Action.UI.Location as DLoc
import Domain.Action.UI.Ride as DRide
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingStatus as DTaxiBooking
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateStatus as DTaxiEstimate
import Domain.Types.Extra.Ride as DRide
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSTicketBooking as DFRFSBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.JourneyLegMapping as DJLM
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.LocationAddress as LA
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RideStatus as DTaxiRide
import qualified Domain.Types.RiderConfig
import qualified Domain.Types.RouteDetails as DRouteDetails
import qualified Domain.Types.Trip as DTrip
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface as KMultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude as KP
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
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
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.GateInfo as GD
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.Offer as SOffer
import SharedLogic.Search
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchOpCity
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyExtra as QJourneyExtra
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.JourneyLegMapping as QJourneyLegMapping
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
                integratedBppConfig <- SIBC.findIntegratedBPPConfig Nothing riderConfig.merchantOperatingCityId Enums.BUS DIBC.MULTIMODAL
                routeWithBuses <- CQMMB.getRoutesBuses routeId integratedBppConfig

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
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  m (Maybe (DJourney.Journey, [DJourneyLeg.JourneyLeg]))
init journeyReq userPreferences blacklistedServiceTiers blacklistedFareQuoteTypes = do
  journeyId <- Common.generateGUID
  riderConfig <- QRC.findByMerchantOperatingCityId journeyReq.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist journeyReq.merchantOperatingCityId.getId)
  searchReq <- QSearchRequest.findById journeyReq.parentSearchId >>= fromMaybeM (SearchRequestNotFound journeyReq.parentSearchId.getId)
  let fromLocation = searchReq.fromLocation
  let toLocation = searchReq.toLocation
  let legsWithContext = zip3 (Nothing : map Just journeyReq.legs) journeyReq.legs (map Just (KP.tail journeyReq.legs) ++ [Nothing])
  legsAndFares <-
    mapWithIndex
      ( \idx (mbPrev, leg, mbNext) -> do
          let travelMode = convertMultiModalModeToTripMode leg.mode (straightLineDistance leg) journeyReq.maximumWalkDistance
          legFare@(_, mbTotalLegFare) <- measureLatency (JLI.getFare leg.fromArrivalTime journeyReq.personId journeyReq.merchantId journeyReq.merchantOperatingCityId journeyReq.routeLiveInfo leg travelMode (Just journeyReq.parentSearchId.getId) blacklistedServiceTiers blacklistedFareQuoteTypes journeyReq.isSingleMode) "multimodal getFare"
          let onboardedSingleModeVehicle =
                if travelMode `elem` [DTrip.Bus, DTrip.Metro, DTrip.Subway]
                  then
                    journeyReq.routeLiveInfo <&> \liveInfo ->
                      JL.FinalBoardedBusData
                        { busNumber = Just liveInfo.vehicleNumber,
                          depotNo = liveInfo.depot,
                          waybillId = liveInfo.waybillId,
                          scheduleNo = liveInfo.scheduleNo,
                          updateSource = Just DJourneyLeg.UserSpotBooked,
                          serviceTierType = Just liveInfo.serviceType,
                          busConductorId = liveInfo.busConductorId,
                          busDriverId = liveInfo.busDriverId
                        }
                  else Nothing
          journeyLeg <- JL.mkJourneyLeg idx (mbPrev, leg, mbNext) fromLocation toLocation journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.parentSearchId journeyReq.maximumWalkDistance mbTotalLegFare Nothing onboardedSingleModeVehicle ((.serviceType) <$> journeyReq.routeLiveInfo) journeyReq.busLocationData
          return (legFare, journeyLeg)
      )
      legsWithContext

  let journeyFareLegs@(mbTotalFares, journeyLeg) = unzip legsAndFares
  logDebug $ "[Multimodal - Legs] : Is Multimodal Testing => " <> show riderConfig.multimodalTesting <> ", " <> show journeyFareLegs
  if not riderConfig.multimodalTesting && any (\(isFareMandatory, mbLegFare) -> isFareMandatory && isNothing mbLegFare) mbTotalFares
    then do return Nothing
    else do
      forM_ journeyLeg QJourneyLeg.create
      hasUserPreferredTransitTypesFlag <- hasUserPreferredTransitTypes journeyLeg userPreferences
      hasUserPreferredTransitModesFlag <- hasUserPreferredTransitModes journeyLeg userPreferences
      journey <- JL.mkJourney journeyReq.isSingleMode searchReq.riderId journeyReq.startTime journeyReq.endTime journeyReq.estimatedDistance journeyReq.estimatedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId journeyReq.legs journeyReq.maximumWalkDistance (searchReq.recentLocationId) journeyReq.relevanceScore hasUserPreferredTransitTypesFlag hasUserPreferredTransitModesFlag fromLocation toLocation
      QJourney.create journey
      logDebug $ "journey for multi-modal: " <> show journey
      return $ Just (journey, journeyLeg)
  where
    straightLineDistance leg = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)
    hasUserPreferredTransitTypes legs userPrefs = do
      let relevantLegs = filter (\leg -> leg.mode == DTrip.Bus || leg.mode == DTrip.Subway) legs
          checkLeg leg =
            case leg.mode of
              DTrip.Bus -> checkTransitType userPrefs.busTransitTypes leg.liveVehicleAvailableServiceTypes
              DTrip.Subway -> checkTransitType userPrefs.subwayTransitTypes leg.liveVehicleAvailableServiceTypes
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

multiModalTravelModeToBecknVehicleCategory :: DTrip.MultimodalTravelMode -> Maybe BecknSpec.VehicleCategory
multiModalTravelModeToBecknVehicleCategory = \case
  DTrip.Metro -> Just BecknSpec.METRO
  DTrip.Bus -> Just BecknSpec.BUS
  DTrip.Subway -> Just BecknSpec.SUBWAY
  _ -> Nothing

getAllLegsInfoWithoutSearch ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DPerson.Person ->
  Id DJourney.Journey ->
  m [JL.LegInfo]
getAllLegsInfoWithoutSearch personId journeyId = getAllLegsInfo' personId journeyId False

getAllLegsInfo ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DPerson.Person ->
  Id DJourney.Journey ->
  m [JL.LegInfo]
getAllLegsInfo personId journeyId = getAllLegsInfo' personId journeyId True

getAllLegsInfo' ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DPerson.Person ->
  Id DJourney.Journey ->
  Bool ->
  m [JL.LegInfo]
getAllLegsInfo' personId journeyId checkSearch = do
  whenJourneyUpdateInProgress journeyId $ do
    allLegs <- QJourneyLeg.getJourneyLegs journeyId
    mapMaybeM (getLegInfo personId checkSearch allLegs) allLegs

getLegInfo ::
  JL.GetStateFlow m r c =>
  Id DPerson.Person ->
  Bool ->
  [DJourneyLeg.JourneyLeg] ->
  DJourneyLeg.JourneyLeg ->
  m (Maybe JL.LegInfo)
getLegInfo personId checkSearch journeyLegs journeyLeg = do
  case journeyLeg.legSearchId of
    Just legSearchIdText -> do
      let legSearchId = Id legSearchIdText
      case journeyLeg.mode of
        DTrip.Taxi -> do
          legInfo <- JL.getInfo $ TaxiLegRequestGetInfo $ TaxiLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg}
          when (isNothing legInfo && checkSearch) $ do
            let journeyId = journeyLeg.journeyId
            journey <- QJourney.findByPrimaryKey journeyId >>= fromMaybeM (JourneyNotFound journeyId.getId)
            legs <- QJourneyLeg.getJourneyLegs journeyId
            void $ markJourneyComplete journey legs
          return legInfo
        DTrip.Walk -> JL.getInfo $ WalkLegRequestGetInfo $ WalkLegRequestGetInfoData {journeyLeg = journeyLeg, personId}
        DTrip.Metro -> JL.getInfo $ MetroLegRequestGetInfo $ MetroLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg = journeyLeg, journeyLegs = journeyLegs}
        DTrip.Subway -> JL.getInfo $ SubwayLegRequestGetInfo $ SubwayLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg = journeyLeg, journeyLegs = journeyLegs}
        DTrip.Bus -> JL.getInfo $ BusLegRequestGetInfo $ BusLegRequestGetInfoData {searchId = cast legSearchId, journeyLeg = journeyLeg, journeyLegs = journeyLegs}
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
  [JL.JourneyLegState] ->
  m ()
checkAndMarkTerminalJourneyStatus journey allLegStates = do
  let flattenedLegStates = concatLegStates allLegStates
      isSingleTaxiJourneyLeg = length flattenedLegStates == 1 && (KP.listToMaybe flattenedLegStates <&> (.mode)) == Just DTrip.Taxi -- This Would Be A Single Leg Journey With Only A Taxi Leg.
  go flattenedLegStates isSingleTaxiJourneyLeg
  where
    concatLegStates =
      foldl'
        ( \acc st -> case st of
            JL.Single legState -> [legState] <> acc
            JL.Transit legStates -> legStates <> acc
        )
        []

    isCancelled :: Bool -> JL.JourneyLegStateData -> Bool
    isCancelled isSingleTaxiJourneyLeg legState =
      let cancelledStatuses =
            [JMState.FRFSBooking DFRFSBooking.CANCELLED, JMState.FRFSBooking DFRFSBooking.CANCEL_INITIATED, JMState.FRFSTicket DFRFSTicket.CANCELLED] -- If status is completed, a booking should exist. If it appears here without a booking, it means the booking was cancelled.
              <> if isSingleTaxiJourneyLeg then [JMState.TaxiRide DTaxiRide.CANCELLED, JMState.TaxiBooking DTaxiBooking.CANCELLED, JMState.TaxiEstimate DTaxiEstimate.CANCELLED, JMState.TaxiEstimate DTaxiEstimate.COMPLETED] else [] -- For Single Taxi JourneyLeg, TaxiEstimate/TaxiBooking/TaxiRide Cancelled Should Also be treated as a Cancelled Journey Status.
       in legState.bookingStatus `elem` cancelledStatuses

    isCompleted :: JL.JourneyLegStateData -> Bool
    isCompleted legState = legState.bookingStatus `elem` [JMState.TaxiRide DTaxiRide.COMPLETED, JMState.Feedback JMState.FEEDBACK_PENDING]

    allTrackingFinished :: [JL.JourneyLegStateData] -> Bool
    allTrackingFinished = all (\legState -> legState.trackingStatus == JMState.Finished)

    go flattenedLegStates isSingleTaxiJourneyLeg
      | isSingleTaxiJourneyLeg && all isCompleted flattenedLegStates = updateJourneyStatus journey DJourney.FEEDBACK_PENDING
      | isSingleTaxiJourneyLeg && all (isCancelled isSingleTaxiJourneyLeg) flattenedLegStates = updateJourneyStatus journey DJourney.CANCELLED
      | not isSingleTaxiJourneyLeg && allTrackingFinished flattenedLegStates =
        if any (isCancelled isSingleTaxiJourneyLeg) flattenedLegStates
          then updateJourneyStatus journey DJourney.CANCELLED
          else updateJourneyStatus journey DJourney.FEEDBACK_PENDING
      | otherwise = pure ()

getAllLegsStatus ::
  (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  DJourney.Journey ->
  m [JL.JourneyLegState]
getAllLegsStatus journey = do
  allLegsRawData <- QJourneyLeg.getJourneyLegs journey.id
  riderLastPoints <- getLastThreePoints journey.id
  riderConfig <- getRiderConfig journey
  let busTrackingConfig = fromMaybe defaultBusTrackingConfig riderConfig.busTrackingConfig
  let movementDetected = hasSignificantMovement (map (.latLong) riderLastPoints) busTrackingConfig
  let legsWithNext = zip allLegsRawData $ map Just (KP.tail allLegsRawData) ++ [Nothing]
  logDebug $ "getAllLegsStatus: legsWithNext: " <> show legsWithNext
  (_, legPairs) <- foldlM (processLeg riderLastPoints movementDetected) (Nothing, []) legsWithNext
  let allLegsState = map snd legPairs
  checkAndMarkTerminalJourneyStatus journey allLegsState
  return allLegsState
  where
    getRouteCodeToTrack :: DJourneyLeg.JourneyLeg -> Maybe Text
    getRouteCodeToTrack leg = safeHead leg.routeDetails >>= ((.routeGtfsId) >=> (pure . gtfsIdtoDomainCode))

    processLeg ::
      (JL.GetStateFlow m r c, JL.SearchRequestFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
      [APITypes.RiderLocationReq] ->
      Bool ->
      (Maybe DJourneyLeg.JourneyLeg, [(DJourneyLeg.JourneyLeg, JL.JourneyLegState)]) ->
      (DJourneyLeg.JourneyLeg, Maybe DJourneyLeg.JourneyLeg) ->
      m (Maybe DJourneyLeg.JourneyLeg, [(DJourneyLeg.JourneyLeg, JL.JourneyLegState)])
    processLeg riderLastPoints movementDetected (_lastLeg, legsState) (leg, _mbNextLeg) = do
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
        Nothing -> throwError $ JourneyLegSearchIdNotFound leg.journeyId.getId leg.sequenceNumber

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
            sortingType = convertSortingType $ fromMaybe DMP.FASTEST userPreferences.journeyOptionsSortingType,
            walkSpeed = Nothing
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
  Id DPerson.Person ->
  [APITypes.JourneyConfirmReqElement] ->
  Maybe Int ->
  DJourney.Journey ->
  Maybe Bool ->
  Maybe Bool ->
  m ()
startJourney riderId confirmElements forcedBookedLegOrder journey mbEnableOffer mbIsMockPayment = do
  allLegs <- getAllLegsInfo riderId journey.id
  mapM_ (\leg -> QTBooking.updateOnInitDoneBySearchId (Just False) (Id leg.searchId)) allLegs -- TODO :: Handle the case where isMultiAllowed is False
  mapM_
    ( \leg -> do
        let mElement = find (\element -> element.journeyLegOrder == leg.order) confirmElements
            ticketQuantity = mElement >>= (.ticketQuantity)
            childTicketQuantity = mElement >>= (.childTicketQuantity)
            bookLater = fromMaybe False (mElement <&> (.skipBooking))
        let forcedBooking = Just leg.order == forcedBookedLegOrder
        let crisSdkResponse = find (\element -> element.journeyLegOrder == leg.order) confirmElements >>= (.crisSdkResponse)
        categorySelectionReq <- do
          let categorySelectionReq' = fromMaybe [] $ find (\element -> element.journeyLegOrder == leg.order) confirmElements >>= (.categorySelectionReq)
          if null categorySelectionReq' && leg.travelMode `elem` [DTrip.Metro, DTrip.Subway, DTrip.Bus]
            then
              maybe
                (pure categorySelectionReq')
                ( \pricingId -> do
                    quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId (Id pricingId)
                    let selectedQuoteCategories =
                          map
                            ( \quoteCategory ->
                                let quantity =
                                      fromMaybe quoteCategory.selectedQuantity
                                        case quoteCategory.category of
                                          ADULT -> ticketQuantity
                                          CHILD -> childTicketQuantity
                                          _ -> Just quoteCategory.selectedQuantity
                                 in APITypes.FRFSCategorySelectionReq {quoteCategoryId = quoteCategory.id, quantity}
                            )
                            quoteCategories
                    return $ categorySelectionReq' <> selectedQuoteCategories
                )
                leg.pricingId
            else return categorySelectionReq'
        let totalTicketQuantity = sum $ map (.quantity) categorySelectionReq
            bookingAllowed' = leg.bookingAllowed || ((fromMaybe False leg.hasApplicablePasses) && totalTicketQuantity /= 1)
            updatedLeg = leg {JL.bookingAllowed = bookingAllowed'}
        JLI.confirm forcedBooking bookLater updatedLeg crisSdkResponse categorySelectionReq journey.isSingleMode mbEnableOffer mbIsMockPayment
    )
    allLegs

startJourneyLeg ::
  (JL.ConfirmFlow m r c, JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) => JL.LegInfo -> Maybe Bool -> m ()
startJourneyLeg legInfo isSingleMode = do
  (categories, crisSdkResponse) <-
    case legInfo.legExtraInfo of
      JL.Metro legExtraInfo -> return (legExtraInfo.categories, Nothing)
      JL.Subway legExtraInfo -> do
        mbBooking <- QTBooking.findBySearchId (Id legInfo.searchId)
        let crisSdkResponse =
              case (mbBooking >>= (.bookingAuthCode), mbBooking >>= (.osType), mbBooking >>= (.osBuildVersion)) of
                (Just bookingAuthCode, Just osType, Just osBuildVersion) -> Just APITypes.CrisSdkResponse {bookAuthCode = bookingAuthCode, osType = osType, osBuildVersion = osBuildVersion, latency = Nothing}
                _ -> Nothing
        return (legExtraInfo.categories, crisSdkResponse)
      JL.Bus legExtraInfo -> return (legExtraInfo.categories, Nothing)
      _ -> return ([], Nothing)
  when (legInfo.travelMode `elem` [DTrip.Metro, DTrip.Subway, DTrip.Bus]) $ do
    QTBooking.updateOnInitDoneBySearchId (Just False) (Id legInfo.searchId)
  let categorySelectionReq =
        map
          ( \category -> APITypes.FRFSCategorySelectionReq {quoteCategoryId = category.categoryId, quantity = category.categorySelectedQuantity}
          )
          categories
  JLI.confirm True False legInfo crisSdkResponse categorySelectionReq isSingleMode Nothing Nothing

addAllLegs ::
  ( JL.SearchRequestFlow m r c,
    JL.GetStateFlow m r c,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  DJourney.Journey ->
  Maybe [DJourneyLeg.JourneyLeg] ->
  [DJourneyLeg.JourneyLeg] ->
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  m ()
addAllLegs journey mbOldJourneyLegs newJourneyLegs blacklistedServiceTiers blacklistedFareQuoteTypes = do
  oldLegs <- maybe (QJourneyLeg.getJourneyLegs journey.id) pure mbOldJourneyLegs
  let filteredOldLegs = filter (\leg1 -> all (\leg2 -> leg1.sequenceNumber /= leg2.sequenceNumber) newJourneyLegs) oldLegs
  let allLegs = sortBy (comparing (.sequenceNumber)) (filteredOldLegs ++ newJourneyLegs)
  toLocation <- journey.toLocation & fromMaybeM (InvalidRequest "To location nothing for Journey / Parent Search Request")
  forM_ (traverseWithTriplets allLegs) $ \(mbPrevJourneyLeg, journeyLeg, mbNextJourneyLeg) -> do
    when (isNothing journeyLeg.legSearchId) $ do
      -- In case of retry of this function, if search has already triggered then it will not do it again
      case journeyLeg.mode of
        DTrip.Taxi -> do
          snappedLeg <- snapJourneyLegToNearestGate journeyLeg
          let originAddress = mkAddress (mbPrevJourneyLeg >>= (.toStopDetails)) journeyLeg.mode journey.fromLocation.address
          let destinationAddress = mkAddress (mbNextJourneyLeg >>= (.fromStopDetails)) journeyLeg.mode toLocation.address
          void $ addTaxiLeg journey snappedLeg originAddress destinationAddress (upsertJourneyLegAction journeyLeg)
        DTrip.Metro -> do
          void $ addMetroLeg journey journeyLeg (upsertJourneyLegAction journeyLeg) blacklistedServiceTiers blacklistedFareQuoteTypes
        DTrip.Subway -> do
          void $ addSubwayLeg journey journeyLeg (upsertJourneyLegAction journeyLeg) blacklistedServiceTiers blacklistedFareQuoteTypes
        DTrip.Walk ->
          upsertJourneyLegAction journeyLeg journeyLeg.id.getId
        DTrip.Bus -> do
          void $ addBusLeg journey journeyLeg journeyLeg.userBookedBusServiceTierType (upsertJourneyLegAction journeyLeg) blacklistedServiceTiers blacklistedFareQuoteTypes
  where
    upsertJourneyLegAction :: JL.SearchRequestFlow m r c => DJourneyLeg.JourneyLeg -> Text -> m ()
    upsertJourneyLegAction journeyLeg searchId = upsertJourneyLeg (journeyLeg {DJourneyLeg.legSearchId = Just searchId})

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

upsertJourneyLeg :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DJourneyLeg.JourneyLeg -> m ()
upsertJourneyLeg journeyLeg = do
  (QJourneyLeg.findById journeyLeg.id) >>= \case
    Just _ -> do
      QJourneyLeg.updateByPrimaryKey journeyLeg
      mbJourneyLegMapping <- getJourneyLegMapping
      whenJust mbJourneyLegMapping QJourneyLegMapping.updateByPrimaryKey
    Nothing -> do
      QJourneyLeg.create journeyLeg
  where
    getJourneyLegMapping = do
      mbExistingMapping <- QJourneyLegMapping.findByJourneyLegId journeyLeg.id
      now <- getCurrentTime
      mapM
        ( \jlm -> do
            return $
              DJLM.JourneyLegMapping
                { id = jlm.id,
                  journeyLegId = journeyLeg.id,
                  journeyId = journeyLeg.journeyId,
                  sequenceNumber = journeyLeg.sequenceNumber,
                  isDeleted = fromMaybe jlm.isDeleted journeyLeg.isDeleted,
                  merchantId = journeyLeg.merchantId,
                  merchantOperatingCityId = journeyLeg.merchantOperatingCityId,
                  createdAt = jlm.createdAt,
                  updatedAt = now
                }
        )
        mbExistingMapping

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
          let nearestResp = minimumBy (compare `on` (.distance)) distanceResponses
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
  (forall m1 r1 c1. JL.SearchRequestFlow m1 r1 c1 => Text -> m1 ()) ->
  m JL.SearchResponse
addTaxiLeg journey journeyLeg originAddress destinationAddress upsertJourneyLegAction = do
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
            multimodalSearchRequestId = Just journey.searchRequestId,
            ..
          }

addMetroLeg ::
  (JL.SearchRequestFlow m r c, JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  (forall m1 r1 c1. JL.SearchRequestFlow m1 r1 c1 => Text -> m1 ()) ->
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  m JL.SearchResponse
addMetroLeg journey journeyLeg upsertJourneyLegAction blacklistedServiceTiers blacklistedFareQuoteTypes = do
  merchantOperatingCity <- QMerchOpCity.findById journey.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound journey.merchantOperatingCityId.getId)
  let metroSearchReq = mkMetroLegReq merchantOperatingCity.city
  JL.search metroSearchReq
  where
    mkMetroLegReq city = do
      MetroLegRequestSearch $
        MetroLegRequestSearchData
          { quantity = 1,
            personId = journey.riderId,
            merchantId = journey.merchantId,
            recentLocationId = journey.recentLocationId,
            multimodalSearchRequestId = Just journey.searchRequestId,
            city,
            journeyLeg,
            upsertJourneyLegAction,
            blacklistedServiceTiers = blacklistedServiceTiers,
            blacklistedFareQuoteTypes = blacklistedFareQuoteTypes,
            isSingleMode = fromMaybe False journey.isSingleMode
          }

addSubwayLeg ::
  JL.SearchRequestFlow m r c =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  (forall m1 r1 c1. JL.SearchRequestFlow m1 r1 c1 => Text -> m1 ()) ->
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  m JL.SearchResponse
addSubwayLeg journey journeyLeg upsertJourneyLegAction blacklistedServiceTiers blacklistedFareQuoteTypes = do
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
            multimodalSearchRequestId = Just journey.searchRequestId,
            city,
            journeyLeg,
            upsertJourneyLegAction,
            blacklistedServiceTiers = blacklistedServiceTiers,
            blacklistedFareQuoteTypes = blacklistedFareQuoteTypes,
            isSingleMode = fromMaybe False journey.isSingleMode
          }

addBusLeg ::
  JL.SearchRequestFlow m r c =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  Maybe Spec.ServiceTierType ->
  (forall m1 r1 c1. JL.SearchRequestFlow m1 r1 c1 => Text -> m1 ()) ->
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  m JL.SearchResponse
addBusLeg journey journeyLeg mbServiceTier upsertJourneyLegAction blacklistedServiceTiers blacklistedFareQuoteTypes = do
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
            multimodalSearchRequestId = Just journey.searchRequestId,
            city,
            journeyLeg,
            upsertJourneyLegAction,
            serviceTier = mbServiceTier,
            blacklistedServiceTiers = blacklistedServiceTiers,
            blacklistedFareQuoteTypes = blacklistedFareQuoteTypes,
            isSingleMode = fromMaybe False journey.isSingleMode
          }

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

deleteLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  DJourneyLeg.JourneyLeg ->
  SCR.CancellationReasonCode ->
  Bool ->
  Maybe (Id DEstimate.Estimate) ->
  m ()
deleteLeg journeyLeg cancellationReasonCode shouldUpdateJourneyStatus cancelEstimateId = do
  cancelLeg journeyLeg cancellationReasonCode shouldUpdateJourneyStatus cancelEstimateId
  QJourneyLegMapping.updateIsDeleted True journeyLeg.id

softCancelLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  DJourneyLeg.JourneyLeg ->
  SCR.CancellationReasonCode ->
  Bool ->
  Maybe (Id DEstimate.Estimate) ->
  m ()
softCancelLeg journeyLeg cancellationReasonCode shouldUpdateJourneyStatus cancelEstimateId = do
  cancelLegUtil journeyLeg cancellationReasonCode shouldUpdateJourneyStatus cancelEstimateId Spec.SOFT_CANCEL

cancelLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  DJourneyLeg.JourneyLeg ->
  SCR.CancellationReasonCode ->
  Bool ->
  Maybe (Id DEstimate.Estimate) ->
  m ()
cancelLeg journeyLeg cancellationReasonCode shouldUpdateJourneyStatus cancelEstimateId = do
  cancelLegUtil journeyLeg cancellationReasonCode shouldUpdateJourneyStatus cancelEstimateId Spec.CONFIRM_CANCEL

cancelLegUtil ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  DJourneyLeg.JourneyLeg ->
  SCR.CancellationReasonCode ->
  Bool ->
  Maybe (Id DEstimate.Estimate) ->
  Spec.CancellationType ->
  m ()
cancelLegUtil journeyLeg cancellationReasonCode shouldUpdateJourneyStatus cancelEstimateId cancellationType = do
  whenJust journeyLeg.legSearchId $ \searchId -> do
    case journeyLeg.mode of
      DTrip.Taxi ->
        JL.cancel $
          TaxiLegRequestCancel
            TaxiLegRequestCancelData
              { searchRequestId = Id searchId,
                reasonCode = cancellationReasonCode,
                additionalInfo = Nothing,
                reallocate = Nothing,
                blockOnCancellationRate = Nothing,
                cancellationSource = SBCR.ByUser,
                cancelEstimateId,
                journeyLeg
              }
      DTrip.Walk ->
        JL.cancel $
          WalkLegRequestCancel
            WalkLegRequestCancelData
              { journeyLegId = journeyLeg.id
              }
      DTrip.Metro ->
        JL.cancel $
          MetroLegRequestCancel
            MetroLegRequestCancelData
              { searchId = Id searchId,
                cancellationType
              }
      DTrip.Subway ->
        JL.cancel $
          SubwayLegRequestCancel
            SubwayLegRequestCancelData
              { searchId = Id searchId,
                cancellationType
              }
      DTrip.Bus ->
        JL.cancel $
          BusLegRequestCancel
            BusLegRequestCancelData
              { searchId = Id searchId,
                cancellationType
              }
  when shouldUpdateJourneyStatus $ do
    journey <- getJourney journeyLeg.journeyId
    updatedLegStatus <- getAllLegsStatus journey
    logError $ "Checking and marking terminal journey status for journey: " <> show journey.id.getId <> " with updatedLegStatus: " <> show (length updatedLegStatus)
    when (length updatedLegStatus == 1) $ do
      checkAndMarkTerminalJourneyStatus journey updatedLegStatus
  return ()

multimodalLegSearchIdAccessLockKey :: Text -> Text
multimodalLegSearchIdAccessLockKey legSearchId = "Multimodal:Leg:SearchIdAccess:" <> legSearchId

canBeSwitched ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    MonadFlow m
  ) =>
  DJourneyLeg.JourneyLeg ->
  DTrip.MultimodalTravelMode ->
  m Bool
canBeSwitched journeyLeg newMode = do
  let currentMode = journeyLeg.mode
  case (currentMode, newMode) of
    (DTrip.Walk, DTrip.Taxi) -> return True
    (DTrip.Taxi, DTrip.Walk) -> return True
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
    QJourneyExtra.updateStatusAndEndTime newStatus journey.id

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
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  m ()
extendLeg journeyId startPoint mbEndLocation mbEndLegOrder fare newDistance newDuration bookingUpdateReqId blacklistedServiceTiers blacklistedFareQuoteTypes = do
  journey <- getJourney journeyId
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for journeyId: " <> show journey.id.getId) journey.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation
  allLegs <- QJourneyLeg.getJourneyLegs journeyId
  now <- getCurrentTime
  case startPoint of
    JL.StartLegOrder startLegOrder -> do
      currentLeg <- find (\leg -> leg.sequenceNumber == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)
      legsToCancel <-
        case mbEndLegOrder of
          Just endLegOrder -> return $ filter (\leg -> leg.sequenceNumber >= startLegOrder && leg.sequenceNumber < endLegOrder) allLegs
          Nothing -> return $ filter (\leg -> leg.sequenceNumber >= startLegOrder) allLegs
      leg <- mkMultiModalTaxiLeg newDistance newDuration MultiModalTypes.Unspecified currentLeg.startLocation.latitude currentLeg.startLocation.longitude endLocation.lat endLocation.lon (fromMaybe now currentLeg.fromArrivalTime)
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg startLegOrder (Nothing, leg, Nothing) journey.fromLocation journey.toLocation currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId (Id journey.searchRequestId) riderConfig.maximumWalkDistance (Just fare) Nothing Nothing Nothing []
      withJourneyUpdateInProgress journeyId $ do
        forM_ legsToCancel $ \currLeg -> deleteLeg currLeg (SCR.CancellationReasonCode "") False Nothing
        QJourneyLeg.create journeyLeg
        updateJourneyChangeLogCounter journeyId
        addAllLegs journey Nothing [journeyLeg] blacklistedServiceTiers blacklistedFareQuoteTypes
    -- check this code
    JL.StartLocation startlocation -> do
      currentLeg <- find (\leg -> leg.sequenceNumber == startlocation.legOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startlocation.legOrder)
      case currentLeg.mode of
        DTrip.Taxi -> do
          bookingUpdateRequestId <- bookingUpdateReqId & fromMaybeM (InvalidRequest "bookingUpdateReqId not found")
          void $ DEditLocation.postEditResultConfirm (Just journey.riderId, journey.merchantId) bookingUpdateRequestId
          Redis.setExp mkExtendLegKey currentLeg.id 300 --5 mins
        DTrip.Walk -> extendWalkLeg journey startlocation endLocation currentLeg
        _ -> do
          throwError $ InvalidRequest ("Cannot extend leg for mode: " <> show currentLeg.mode)
  where
    extendWalkLeg journey startlocation endLocation currentLeg = do
      now <- getCurrentTime
      leg <- mkMultiModalTaxiLeg newDistance newDuration MultiModalTypes.Unspecified startlocation.location.lat startlocation.location.lon endLocation.lat endLocation.lon now
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg currentLeg.sequenceNumber (Nothing, leg, Nothing) journey.fromLocation journey.toLocation currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId (Id journey.searchRequestId) riderConfig.maximumWalkDistance (Just fare) Nothing Nothing Nothing []
      withJourneyUpdateInProgress journeyId $ do
        -- fix it properly later
        -- cancelRequiredLegs journey.riderId
        QJourneyLeg.create journeyLeg
        void $ addTaxiLeg journey journeyLeg (mkLocationAddress startlocation.location) (mkLocationAddress endLocation) (\searchId -> QJourneyLeg.updateLegSearchId (Just searchId) journeyLeg.id)
        startJourney journey.riderId [] (Just currentLeg.sequenceNumber) journey Nothing Nothing

    -- cancelRequiredLegs riderId = do
    --   case mbEndLegOrder of
    --     Nothing -> cancelRemainingLegs journeyId False riderId
    --     Just endLegOrder -> do
    --       remainingLegs <- getRemainingLegs journeyId riderId
    --       let legsToCancel = filter (\leg -> leg.order < endLegOrder) remainingLegs
    --       mapM_
    --         ( \leg -> deleteLeg leg (SCR.CancellationReasonCode "") False Nothing
    --         )
    --         legsToCancel

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
            exit = Nothing,
            providerRouteId = Nothing
          }

    mkExtendLegKey = "Extend:Leg:For:JourneyId-" <> journeyId.getId

    mkLocationAddress DLocation.LocationAPIEntity {..} = LA.LocationAddress {..}

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
extendLegEstimatedFare journeyId startPoint mbEndLocation _ = do
  journey <- getJourney journeyId
  allLegs <- QJourneyLeg.getJourneyLegs journeyId
  (currentLeg, startLocation) <- case startPoint of
    JL.StartLegOrder startLegOrder -> do
      leg <- find (\l -> l.sequenceNumber == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)
      return (leg, LatLong {lat = leg.startLocation.latitude, lon = leg.startLocation.longitude})
    JL.StartLocation startLocation -> do
      leg <- find (\l -> l.sequenceNumber == startLocation.legOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLocation.legOrder)
      return (leg, LatLong {lat = startLocation.location.lat, lon = startLocation.location.lon})
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for journeyId: " <> show journey.id.getId) journey.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation
  case currentLeg.mode of
    DTrip.Taxi -> do
      mbBooking <- maybe (pure Nothing) QBooking.findByTransactionId currentLeg.legSearchId
      mbRide <- maybe (pure Nothing) (QRide.findByRBId . (.id)) mbBooking
      case mbRide of
        Just ride -> do
          let editLocReq =
                DRide.EditLocationReq
                  { origin = Nothing,
                    destination = Just $ DRide.EditLocation {gps = LatLong {lat = endLocation.lat, lon = endLocation.lon}, address = getAddress endLocation}
                  }
          editLocResp <- DRide.editLocation ride.id (journey.riderId, currentLeg.merchantId) editLocReq -- handle case if driver declines
          case editLocResp.bookingUpdateRequestId of
            Just bookingUpdateReqId -> do
              searchForUpdateRequest bookingUpdateReqId (5 :: Int) -- can set in config
            Nothing -> throwError (InvalidRequest "bookingUpdateRequestId not found")
        Nothing -> getUpdatedFare journey currentLeg startLocation endLocation
    _ -> getUpdatedFare journey currentLeg startLocation endLocation
  where
    getUpdatedFare ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        EncFlow m r,
        Monad m,
        m ~ Kernel.Types.Flow.FlowR AppEnv
      ) =>
      DJourney.Journey ->
      DJourneyLeg.JourneyLeg ->
      LatLong ->
      DLocation.LocationAPIEntity ->
      m APITypes.ExtendLegGetFareResp
    getUpdatedFare journey currentLeg startLocation endLocation = do
      distResp <-
        Maps.getDistance currentLeg.merchantId currentLeg.merchantOperatingCityId (Just journeyId.getId) $
          Maps.GetDistanceReq
            { origin = startLocation,
              destination = LatLong {lat = endLocation.lat, lon = endLocation.lon},
              travelMode = Just Maps.CAR,
              sourceDestinationMapping = Nothing,
              distanceUnit = Meter
            }
      let distance = convertMetersToDistance Meter distResp.distance
      now <- getCurrentTime
      let multiModalLeg = mkMultiModalTaxiLeg distance distResp.duration MultiModalTypes.Unspecified startLocation.lat startLocation.lon endLocation.lat endLocation.lon
      (isFareMandatory, estimatedFare) <- JLI.getFare (Just now) journey.riderId currentLeg.merchantId currentLeg.merchantOperatingCityId Nothing multiModalLeg DTrip.Taxi Nothing [] [] (fromMaybe False journey.isSingleMode)
      when (isFareMandatory && isNothing estimatedFare) $ throwError (InvalidRequest "Fare is mandatory for this leg, but unavailable")
      return $
        APITypes.ExtendLegGetFareResp
          { totalFare = estimatedFare,
            distance = distance,
            duration = Just distResp.duration,
            bookingUpdateRequestId = Nothing
          }

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
              { totalFare = Just JL.GetFareResponse {estimatedMinFare = estimatedFare, estimatedMaxFare = estimatedFare, liveVehicleAvailableServiceTypes = Nothing, possibleRoutes = Nothing},
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
          exit = Nothing,
          providerRouteId = Nothing
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
  Id DPerson.Person ->
  APITypes.SwitchLegReq ->
  Maybe Bool ->
  m ()
switchLeg journeyId _ req filterServiceAndJrnyType = do
  journeyLeg <- QJourneyLeg.getJourneyLeg journeyId req.legOrder
  canSwitch <- canBeSwitched journeyLeg req.newMode
  unless canSwitch $ do throwError (JourneyLegCannotBeSwitched journeyLeg.id.getId)
  let blacklistedServiceTiers = if filterServiceAndJrnyType == Just False then [] else [Spec.AC_EMU_FIRST_CLASS]
  let blacklistedFareQuoteTypes = if filterServiceAndJrnyType == Just False then [] else [DFRFSQuote.ReturnJourney]
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
        -- Use OSRM distance but calculate duration using our correct formula
        let calculatedDuration = calculateWalkDuration newDistanceAndDuration.distanceWithUnit
        return (Just newDistanceAndDuration.distanceWithUnit, Just calculatedDuration)
      _ -> return (journeyLeg.distance, journeyLeg.duration)
  let lockKey = multimodalLegSearchIdAccessLockKey journeyId.getId
  Redis.whenWithLockRedis lockKey 5 $ do
    deleteLeg journeyLeg (SCR.CancellationReasonCode "") False Nothing
    newJourneyLeg <- updateJourneyLeg journeyLeg req.newMode startLocation newDistance newDuration
    journey <- getJourney journeyId
    addAllLegs journey Nothing [newJourneyLeg] blacklistedServiceTiers blacklistedFareQuoteTypes
  where
    updateJourneyLeg ::
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
    updateJourneyLeg journeyLeg newMode startLocation newDistance newDuration = do
      now <- getCurrentTime
      journeyLegId <- generateGUID
      return $
        journeyLeg
          { DJourneyLeg.distance = newDistance,
            DJourneyLeg.duration = newDuration,
            DJourneyLeg.fromStopDetails =
              case req.originAddress of
                Just originAddress ->
                  case originAddress.area <|> originAddress.title of
                    Just name ->
                      Just
                        MultiModalTypes.MultiModalStopDetails
                          { stopCode = Nothing,
                            platformCode = Nothing,
                            name = Just name,
                            gtfsId = Nothing
                          }
                    Nothing -> journeyLeg.fromStopDetails
                Nothing -> journeyLeg.fromStopDetails,
            DJourneyLeg.id = journeyLegId,
            DJourneyLeg.routeDetails = (\routeDetail -> routeDetail {DRouteDetails.journeyLegId = journeyLegId.getId, DRouteDetails.trackingStatus = Nothing}) <$> journeyLeg.routeDetails,
            DJourneyLeg.mode = newMode,
            DJourneyLeg.liveVehicleAvailableServiceTypes = Nothing,
            DJourneyLeg.startLocation = startLocation,
            DJourneyLeg.toArrivalTime = Nothing,
            DJourneyLeg.estimatedMinFare = Nothing, -- will be updated by on_search
            DJourneyLeg.estimatedMaxFare = Nothing, -- will be updated by on_search
            DJourneyLeg.legPricingId = Nothing, -- will be updated by on_search
            DJourneyLeg.createdAt = now,
            DJourneyLeg.updatedAt = now,
            DJourneyLeg.legSearchId = Nothing, -- will be updated by add Leg
            DJourneyLeg.isDeleted = Just False
          }

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

generateJourneyInfoResponse :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, ServiceFlow m r, EsqDBReplicaFlow m r, CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m) => DJourney.Journey -> [JL.LegInfo] -> m APITypes.JourneyInfoResp
generateJourneyInfoResponse journey legs = do
  let estimatedMinFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMinFare <&> (.amount)) legs
  let estimatedMaxFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMaxFare <&> (.amount)) legs
  let unifiedQR = getUnifiedQR journey legs
  let mbCurrency = KP.listToMaybe legs >>= (\leg -> leg.estimatedMinFare <&> (.currency))
  merchantOperatingCity <- QMerchOpCity.findById journey.merchantOperatingCityId
  let merchantOperatingCityName = show . (.city) <$> merchantOperatingCity
  let unifiedQRV2 = getUnifiedQRV2 unifiedQR
  offer <-
    withTryCatch "generateJourneyInfoResponse:offerListCache" (SOffer.offerListCache journey.merchantId journey.riderId journey.merchantOperatingCityId DOrder.FRFSMultiModalBooking (mkPrice mbCurrency estimatedMinFareAmount))
      >>= \case
        Left _ -> return Nothing
        Right offersResp -> SOffer.mkCumulativeOfferResp journey.merchantOperatingCityId offersResp legs
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
        createdAt = journey.createdAt,
        endTime = journey.endTime,
        merchantOperatingCityName,
        paymentOrderShortId = journey.paymentOrderShortId,
        unifiedQRV2,
        result = Just "Success",
        isSingleMode = journey.isSingleMode,
        offer
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
  DJourney.Journey ->
  [JL.JourneyLegState] ->
  Flow APITypes.JourneyStatusResp
generateJourneyStatusResponse journey legs = do
  journeyChangeLogCounter <- getJourneyChangeLogCounter journey.id
  return $ APITypes.JourneyStatusResp {legs = concatMap transformLeg legs, journeyStatus = journey.status, journeyPaymentStatus = Nothing, journeyChangeLogCounter}
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
              trackingStatusLastUpdatedAt = legData.trackingStatusLastUpdatedAt,
              userPosition = legData.userPosition,
              vehiclePositions = legData.vehiclePositions,
              mode = legData.mode,
              fleetNo = legData.fleetNo
            }

markLegStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe JL.JourneyLegStatus -> Maybe JMState.TrackingStatus -> DJourneyLeg.JourneyLeg -> Maybe Int -> UTCTime -> m ()
markLegStatus mbStatus trackingStatus journeyLeg mbSubLegOrder trackingStatusUpdateTime = do
  let finalStatus = trackingStatus <|> castJourneyLegStatusToTrackingStatus mbStatus
  whenJust finalStatus $ \status -> do
    JMStateUtils.setJourneyLegTrackingStatus journeyLeg mbSubLegOrder status trackingStatusUpdateTime

    -- TODO :: UI is sending subLegOrder as 0 for Taxi and Walk leg but on Backend subLegOrder starts from 1 always in All modes for consistency, but to handle current UI even if subLegOrder is coming as 0 we are updating with 1
    whenJust mbSubLegOrder $ \subLegOrder -> do
      when (subLegOrder == 0) $ do
        JMStateUtils.setJourneyLegTrackingStatus journeyLeg (Just 1) status trackingStatusUpdateTime
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
      Just JL.Skipped -> Just JMState.InPlan
      Just JL.Cancelled -> Just JMState.Finished
      Just JL.Completed -> Just JMState.Finished
      Just JL.Failed -> Just JMState.Finished
      Nothing -> Nothing

-- Constants for walk duration calculation
averageSpeedMPSForWalk :: Double
averageSpeedMPSForWalk = 1.39 -- WALK speed in m/s

-- for 2km it takes around 29 minutes
fudgeFactorForWalk :: Double
fudgeFactorForWalk = 1.2

calculateWalkDuration :: Distance -> Seconds
calculateWalkDuration distance =
  let distanceInMeters = distanceToMeters distance
      -- Formula: (distance * fudgeFactor) / averageSpeedMPS
      -- This gives us the correct walk duration in seconds
      walkDurationInSeconds = round $ (fromIntegral distanceInMeters * fudgeFactorForWalk) / averageSpeedMPSForWalk
   in Seconds walkDurationInSeconds

cancelOngoingTaxiLegs ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  [DJourneyLeg.JourneyLeg] ->
  m ()
cancelOngoingTaxiLegs =
  mapM_
    ( \leg -> do
        case leg.mode of
          DTrip.Taxi -> cancelLeg leg (SCR.CancellationReasonCode "") False Nothing
          _ -> return ()
    )

checkIfAnyTaxiLegOngoing :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => [DJourneyLeg.JourneyLeg] -> m ()
checkIfAnyTaxiLegOngoing legs = do
  ongoings <- mapM isTaxiLegOngoing legs
  when (or ongoings) $
    throwError (InvalidRequest "You have an Ongoing Taxi Ride. Please complete it before proceeding.")

isTaxiLegOngoing :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> m Bool
isTaxiLegOngoing journeyLeg = do
  case (journeyLeg.legSearchId, journeyLeg.mode) of
    (Just legSearchId, DTrip.Taxi) -> do
      mbBooking <- QBooking.findByTransactionIdAndStatus legSearchId DBooking.activeBookingStatus
      return $ isJust mbBooking
    _ -> return False

markJourneyComplete :: DJourney.Journey -> [DJourneyLeg.JourneyLeg] -> Flow [JL.JourneyLegState]
markJourneyComplete journey legs = do
  cancelOngoingTaxiLegs legs
  now <- getCurrentTime
  mapM_ (\leg -> markAllSubLegsCompleted leg now) legs
  updatedLegStatus <- getAllLegsStatus journey
  checkAndMarkTerminalJourneyStatus journey updatedLegStatus
  return updatedLegStatus
  where
    -- Helper function to mark all sub-legs as completed for FRFS legs
    markAllSubLegsCompleted :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> UTCTime -> m ()
    markAllSubLegsCompleted journeyLeg trackingStatusUpdateTime = do
      let subLegOrders = map (\r -> fromMaybe 1 r.subLegOrder) journeyLeg.routeDetails
      case subLegOrders of
        [] -> markLegStatus (Just JL.Completed) (Just JMState.Finished) journeyLeg Nothing trackingStatusUpdateTime
        orders -> mapM_ (\subOrder -> markLegStatus (Just JL.Completed) (Just JMState.Finished) journeyLeg (Just subOrder) trackingStatusUpdateTime) orders

switchFRFSQuoteTier ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  DJourneyLeg.JourneyLeg ->
  Id DFRFSQuote.FRFSQuote ->
  m ()
switchFRFSQuoteTier journeyLeg quoteId = switchFRFSQuoteTierUtil journeyLeg quoteId

getLegTierOptions ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  DJourneyLeg.JourneyLeg ->
  Bool ->
  m [DRouteDetails.AvailableRoutesByTier]
getLegTierOptions journeyLeg enableSuburbanRoundTrip = getLegTierOptionsUtil journeyLeg enableSuburbanRoundTrip
