module Lib.JourneyLeg.Common.FRFS where

import qualified API.Types.UI.FRFSTicketService as API
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.OnSelect as DOnSelect
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Types.FRFSQuote
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RecentLocation as DRL
import qualified Domain.Types.RiderConfig as DomainRiderConfig
import qualified Domain.Types.RouteDetails as RD
import Domain.Types.RouteStopMapping (RouteStopMapping)
import Domain.Types.Station
import Domain.Types.Trip as DTrip
import Domain.Utils (mapConcurrently)
import ExternalBPP.CallAPI as CallExternalBPP
import ExternalBPP.ExternalAPI.CallAPI as CallAPI
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
-- import qualified Lib.JourneyLeg.Types as JPT
import qualified Lib.JourneyModule.State.Types as JMStateTypes
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import qualified Lib.JourneyModule.Types as JT
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.CachedQueries.Merchant.MultiModalBus (BusData (..), BusDataWithRoutesInfo (..), FullBusData (..))
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Tools.Error

-- getState and other functions from the original file...

getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => DTrip.MultimodalTravelMode -> Id FRFSSearch -> [APITypes.RiderLocationReq] -> Bool -> Maybe Text -> DJourneyLeg.JourneyLeg -> m JT.JourneyLegState
getState mode searchId riderLastPoints movementDetected routeCodeForDetailedTracking journeyLeg = do
  logDebug $ "CFRFS getState: searchId: " <> searchId.getId <> ", mode: " <> show mode
  mbBooking <- QTBooking.findBySearchId searchId
  now <- getCurrentTime
  let userPosition = (.latLong) <$> listToMaybe riderLastPoints
  case mbBooking of
    Just booking -> do
      integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
      (oldStatus, bookingStatus, trackingStatuses) <- JMStateUtils.getFRFSAllStatuses journeyLeg (Just booking)
      case mode of
        DTrip.Bus -> do
          logDebug $ "CFRFS getState: Processing Bus leg for booking with searchId: " <> show searchId.getId
          mbCurrentLegDetails <- QJourneyLeg.findByLegSearchId (Just searchId.getId)

          let bookedForRouteId = routeCodeForDetailedTracking <|> (mbCurrentLegDetails >>= (listToMaybe . (.routeDetails)) >>= (.routeGtfsId) <&> gtfsIdtoDomainCode)

          let routesToUseForTrackVehicles = concat (mapMaybe (.alternateRouteIds) journeyLeg.routeDetails) <> (fromMaybe [] $ bookedForRouteId <&> (: []))

          -- Fetch all bus data for the route using getRoutesBuses
          (allBusDataForRoute, routeStopMappings) <- do
            buses <- concat <$> mapConcurrently (\rc -> (.buses) <$> CQMMB.getRoutesBuses rc) routesToUseForTrackVehicles
            routeStopMappings <- HM.fromList <$> mapM (\rc -> (rc,) <$> HM.fromList . map (\a -> (a.stopCode, a)) <$> OTPRest.getRouteStopMappingByRouteCode rc integratedBppConfig) routesToUseForTrackVehicles
            return (buses, routeStopMappings)

          -- Fetch user's boarding station and leg's end station details
          mbUserBoardingStation <- OTPRest.getStationByGtfsIdAndStopCode booking.fromStationCode integratedBppConfig
          mbLegEndStation <- OTPRest.getStationByGtfsIdAndStopCode booking.toStationCode integratedBppConfig
          logDebug $ "CFRFS getState: Processing Bus leg for booking with mbUserBoardingStation:" <> show mbUserBoardingStation <> "mbLegEndStation: " <> show mbLegEndStation
          let (trackingStatus, trackingStatusLastUpdatedAt) =
                case listToMaybe trackingStatuses of
                  Just (_, ts, tsupAt) -> (ts, tsupAt)
                  Nothing -> (JMStateTypes.InPlan, now)
          let baseStateData =
                JT.JourneyLegStateData
                  { status = oldStatus,
                    bookingStatus,
                    trackingStatus,
                    trackingStatusLastUpdatedAt,
                    userPosition,
                    JT.vehiclePositions = [], -- Will be populated based on status
                    legOrder = journeyLeg.sequenceNumber,
                    subLegOrder = 1,
                    mode,
                    fleetNo = mbCurrentLegDetails >>= (.finalBoardedBusNumber)
                  }
          mbQuote <- QFRFSQuote.findById booking.quoteId
          validBuses <-
            case mbQuote of
              Just quote -> do
                let routeStations :: Maybe [API.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
                let mbServiceTier = listToMaybe $ mapMaybe (.vehicleServiceTier) (fromMaybe [] routeStations)
                case mbServiceTier of
                  Just serviceTier -> do
                    riderConfig <- QRiderConfig.findByMerchantOperatingCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
                    let allowedVariants = maybe (defaultBusBoardingRelationshitCfg serviceTier._type) (.canBoardIn) $ find (\serviceRelationShip -> serviceRelationShip.vehicleType == Enums.BUS && serviceRelationShip.serviceTierType == serviceTier._type) =<< riderConfig.serviceTierRelationshipCfg
                    map fst . filter (\(_bs, mbVehicleServiceTier) -> maybe True (\vehicleServiceTier -> vehicleServiceTier `elem` allowedVariants) mbVehicleServiceTier)
                      <$> mapConcurrently
                        ( \busData -> do
                            vd <- OTPRest.getVehicleServiceType integratedBppConfig busData.vehicleNumber
                            return (busData, vd <&> (.service_type))
                        )
                        allBusDataForRoute
                  Nothing -> pure allBusDataForRoute
              Nothing -> pure allBusDataForRoute
          vehiclePositionsToReturn <-
            processBusLegState
              now
              mbCurrentLegDetails
              bookedForRouteId
              riderLastPoints
              booking.merchantOperatingCityId
              mbUserBoardingStation
              mbLegEndStation
              validBuses
              routeStopMappings
              trackingStatus
              movementDetected

          let detailedStateData = baseStateData {JT.vehiclePositions = vehiclePositionsToReturn}
          -- Commented out: Automatic fleet number finalization based on rider location
          -- finalStateData <- updateFleetNoIfFinalized integratedBppConfig detailedStateData mbCurrentLegDetails searchId (isJust mbBooking)
          -- return $ JT.Single finalStateData
          return $ JT.Single detailedStateData
        _ -> do
          let journeyLegStates =
                [ JT.JourneyLegStateData
                    { status = JMStateUtils.castTrackingStatusToJourneyLegStatus trackingStatus,
                      bookingStatus,
                      trackingStatus,
                      trackingStatusLastUpdatedAt,
                      userPosition,
                      JT.vehiclePositions = [],
                      legOrder = journeyLeg.sequenceNumber,
                      subLegOrder,
                      mode,
                      fleetNo = Nothing
                    }
                  | (subLegOrder, trackingStatus, trackingStatusLastUpdatedAt) <- trackingStatuses
                ]
          return $ JT.Transit journeyLegStates
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity searchReq
      (oldStatus, bookingStatus, trackingStatuses) <- JMStateUtils.getFRFSAllStatuses journeyLeg Nothing
      case mode of
        DTrip.Bus -> do
          mbCurrentLegDetails <- QJourneyLeg.findByLegSearchId (Just searchId.getId)

          let bookedForRouteId = routeCodeForDetailedTracking <|> (mbCurrentLegDetails >>= (listToMaybe . (.routeDetails)) >>= (.routeGtfsId) <&> gtfsIdtoDomainCode)

          let routesToUseForTrackVehicles = concat (mapMaybe (.alternateRouteIds) journeyLeg.routeDetails) <> (fromMaybe [] $ bookedForRouteId <&> (: []))

          -- Fetch all bus data for the route using getRoutesBuses
          (allBusDataForRoute, routeStopMappings) <- do
            buses <- concat <$> mapConcurrently (\rc -> (.buses) <$> CQMMB.getRoutesBuses rc) routesToUseForTrackVehicles
            routeStopMappings <- HM.fromList <$> mapM (\rc -> (rc,) <$> HM.fromList . map (\a -> (a.stopCode, a)) <$> OTPRest.getRouteStopMappingByRouteCode rc integratedBppConfig) routesToUseForTrackVehicles
            return (buses, routeStopMappings)

          -- Fetch user's boarding station and leg's end station details
          mbUserBoardingStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.fromStationCode integratedBppConfig
          mbLegEndStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.toStationCode integratedBppConfig
          let (trackingStatus, trackingStatusLastUpdatedAt) =
                case listToMaybe trackingStatuses of
                  Just (_, ts, tsupAt) -> (ts, tsupAt)
                  Nothing -> (JMStateTypes.InPlan, now)
          let baseStateData =
                JT.JourneyLegStateData
                  { status = oldStatus,
                    bookingStatus,
                    trackingStatus,
                    trackingStatusLastUpdatedAt,
                    userPosition,
                    JT.vehiclePositions = [], -- Will be populated based on status
                    legOrder = journeyLeg.sequenceNumber,
                    subLegOrder = 1,
                    mode,
                    fleetNo = mbCurrentLegDetails >>= (.finalBoardedBusNumber)
                  }

          vehiclePositionsToReturn <-
            processBusLegState
              now
              mbCurrentLegDetails
              bookedForRouteId
              riderLastPoints
              searchReq.merchantOperatingCityId
              mbUserBoardingStation
              mbLegEndStation
              allBusDataForRoute
              routeStopMappings
              trackingStatus
              movementDetected

          let detailedStateData = baseStateData {JT.vehiclePositions = vehiclePositionsToReturn}
          logDebug $ "CFRFS getState: Detailed state data for without booking: " <> show vehiclePositionsToReturn
          -- Commented out: Automatic fleet number finalization based on rider location
          -- finalStateData <- updateFleetNoIfFinalized integratedBppConfig detailedStateData mbCurrentLegDetails searchId (isJust mbBooking)
          -- return $ JT.Single finalStateData
          return $ JT.Single detailedStateData
        _ -> do
          -- Other modes (Metro, Subway, etc.)
          let journeyLegStates =
                [ JT.JourneyLegStateData
                    { status = JMStateUtils.castTrackingStatusToJourneyLegStatus trackingStatus,
                      bookingStatus,
                      trackingStatus,
                      trackingStatusLastUpdatedAt,
                      userPosition,
                      JT.vehiclePositions = [],
                      legOrder = journeyLeg.sequenceNumber,
                      subLegOrder = subLegOrder,
                      mode,
                      fleetNo = Nothing
                    }
                  | (subLegOrder, trackingStatus, trackingStatusLastUpdatedAt) <- trackingStatuses
                ]
          return $ JT.Transit journeyLegStates

-- where
--   updateFleetNoIfFinalized ::
--     (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) =>
--     DIBC.IntegratedBPPConfig ->
--     JT.JourneyLegStateData ->
--     Maybe DJourneyLeg.JourneyLeg ->
--     Id FRFSSearch ->
--     Bool ->
--     m JT.JourneyLegStateData
--   updateFleetNoIfFinalized integratedBppConfig detailedStateData mbCurrentLegDetails searchId' isBooking =
--     if detailedStateData.status `elem` [JPT.Finishing, JPT.Completed]
--       then do
--         case mbCurrentLegDetails of
--           Just legToUpdate -> do
--             case legToUpdate.finalBoardedBusNumber of
--               Nothing -> do
--                 bestCandidateResult <- Hedis.zrevrangeWithscores (topVehicleCandidatesKeyFRFS (legToUpdate.id.getId)) 0 0
--                 case bestCandidateResult of
--                   [] -> pure detailedStateData
--                   ((bestVehicleNumber, _) : _) -> do
--                     mbVehicleRouteInfo <- JMU.getVehicleLiveRouteInfo [integratedBppConfig] bestVehicleNumber
--                     let mbVehicleInfo = mbVehicleRouteInfo <&> snd
--                     QJourneyLeg.updateByPrimaryKey $
--                       legToUpdate
--                         { DJourneyLeg.finalBoardedBusNumber = Just bestVehicleNumber,
--                           DJourneyLeg.finalBoardedBusNumberSource = Just DJourneyLeg.Detected,
--                           DJourneyLeg.finalBoardedDepotNo = mbVehicleInfo >>= (.depot),
--                           DJourneyLeg.finalBoardedWaybillId = mbVehicleInfo >>= (.waybillId),
--                           DJourneyLeg.finalBoardedScheduleNo = mbVehicleInfo >>= (.scheduleNo),
--                           DJourneyLeg.finalBoardedBusServiceTierType = mbVehicleInfo <&> (.serviceType)
--                         }
--                     -- Update in-memory detailedStateData as well
--                     pure (detailedStateData :: JT.JourneyLegStateData) {JT.fleetNo = Just bestVehicleNumber}
--               Just _ -> pure detailedStateData
--           Nothing -> do
--             logError $ "CFRFS getState: Could not find leg to update finalBoardedBusNumber for searchId: " <> searchId'.getId
--             pure detailedStateData
--       else do
--         when isBooking $ logDebug $ "CFRFS getState: Not finalizing state for booking with searchId: " <> searchId'.getId <> "for state: " <> show detailedStateData
--         pure detailedStateData

getFare :: (CoreMetrics m, CacheFlow m r, EncFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, HasField "ltsHedisEnv" r Redis.HedisEnv, HasKafkaProducer r, HasShortDurationRetryCfg r c) => Id DPerson.Person -> DMerchant.Merchant -> MerchantOperatingCity -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> [FRFSRouteDetails] -> Maybe UTCTime -> Maybe Text -> Maybe Text -> m (Bool, Maybe JT.GetFareResponse)
getFare riderId merchant merchantOperatingCity vehicleCategory serviecType routeDetails mbFromArrivalTime agencyGtfsId mbSearchReqId = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromAgency agencyGtfsId merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) DIBC.MULTIMODAL
  case routeDetails of
    [] -> do
      logError $ "No Route Details Found for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
      return (True, Nothing)
    _ -> do
      CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory)
        >>= \case
          Just bapConfig -> do
            let fareRouteDetails = NE.fromList $ mapMaybe (\rd -> (\rc -> CallAPI.BasicRouteDetail {routeCode = rc, startStopCode = rd.startStationCode, endStopCode = rd.endStationCode}) <$> rd.routeCode) routeDetails
            JMU.measureLatency (withTryCatch "getFares:getFaresForRouteDetails" $ Flow.getFares riderId merchant merchantOperatingCity integratedBPPConfig bapConfig fareRouteDetails vehicleCategory serviecType mbSearchReqId) ("getFares" <> show vehicleCategory <> " routeDetails: " <> show fareRouteDetails)
              >>= \case
                Right (isFareMandatory, []) -> do
                  logError $ "Getting Empty Fares for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
                  return (isFareMandatory, Nothing)
                Right (isFareMandatory, fares) -> do
                  now <- getCurrentTime
                  let arrivalTime = fromMaybe now mbFromArrivalTime
                  -- L.setOptionLocal QRSTT.CalledForFare True
                  ((possibleServiceTiers, availableFares), mbPossibleRoutes) <- case serviecType of
                    Just serviceTier -> pure ((Just [serviceTier], fares), Nothing) -- bypassing as in case of serviceType/serviceTier is passed, we only calculate fare for that type
                    Nothing -> JMU.measureLatency (filterAvailableBuses arrivalTime fareRouteDetails integratedBPPConfig fares) ("filterAvailableBuses" <> show vehicleCategory <> " routeDetails: " <> show fareRouteDetails)
                  -- L.setOptionLocal QRSTT.CalledForFare False

                  (mbMinFarePerRoute, mbMaxFarePerRoute) <- case vehicleCategory of
                    Spec.BUS -> do
                      mbSelectedServiceTierQuote <- JMU.sortAndGetBusFares merchantOperatingCity.id availableFares
                      case mbSelectedServiceTierQuote of
                        Just selectedServiceTierQuote -> pure (Just selectedServiceTierQuote, Just selectedServiceTierQuote)
                        Nothing -> pure (selectMinFare availableFares, selectMaxFare availableFares)
                    _ -> pure (selectMinFare availableFares, selectMaxFare availableFares)
                  logDebug $ "all fares: " <> show fares <> "min fare: " <> show mbMinFarePerRoute <> "max fare: " <> show mbMaxFarePerRoute <> "possible service tiers: " <> show possibleServiceTiers <> "available fares: " <> show availableFares
                  case (mbMinFarePerRoute, mbMaxFarePerRoute) of
                    (Just minFare, Just maxFare) -> do
                      let minAdultFare = maybe (HighPrecMoney 0.0) (.price.amount) (find (\category -> category.category == ADULT) minFare.categories)
                          maxAdultFare = maybe (HighPrecMoney 0.0) (.price.amount) (find (\category -> category.category == ADULT) maxFare.categories)
                      return (isFareMandatory, Just $ JT.GetFareResponse {liveVehicleAvailableServiceTypes = possibleServiceTiers, estimatedMinFare = minAdultFare, estimatedMaxFare = maxAdultFare, possibleRoutes = mbPossibleRoutes})
                    _ -> do
                      logError $ "No Fare Found for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
                      return (isFareMandatory, Nothing)
                Left err -> do
                  logError $ "Exception Occured in Get Fare for Vehicle Category : " <> show vehicleCategory <> ", Error : " <> show err
                  return (True, Nothing)
          Nothing -> do
            logError $ "Did not get Beckn Config for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
            return (False, Nothing)
  where
    filterAvailableBuses :: (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, MonadFlow m, CacheFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv, HasKafkaProducer r, HasShortDurationRetryCfg r c) => UTCTime -> NE.NonEmpty CallAPI.BasicRouteDetail -> DIBC.IntegratedBPPConfig -> [FRFSFare] -> m ((Maybe [Spec.ServiceTierType], [FRFSFare]), Maybe [RD.AvailableRoutesByTier])
    filterAvailableBuses arrivalTime fareRouteDetails integratedBPPConfig fares = do
      case vehicleCategory of
        Spec.BUS -> do
          -- Above getFares function return fares for all types of buses (e.g. AC, Non-AC, Ordinary, etc.) but instead of showing all types of buses to user,
          -- Check for all possible buses available in next hour and just show fares for those buses to avoid confusion
          let startStationCode = (NE.head fareRouteDetails).startStopCode
          let endStationCode = (NE.last fareRouteDetails).endStopCode
          (_, possibleRoutes, _) <- JMU.findPossibleRoutes Nothing startStationCode endStationCode arrivalTime integratedBPPConfig merchant.id merchantOperatingCity.id Enums.BUS True False False
          let selectedFareRouteCodes = mapMaybe (.routeCode) routeDetails
          logDebug $ "filterAvailableBuses: selectedFareRouteCodes = " <> show selectedFareRouteCodes
          logDebug $ "filterAvailableBuses: possibleRoutes count = " <> show (length possibleRoutes) <> ", details = " <> show (map (\r -> (r.serviceTier, map (.routeCode) r.availableRoutesInfo)) possibleRoutes)
          let routeFilterResults =
                map
                  ( \route ->
                      let routeCodes = map (.routeCode) route.availableRoutesInfo
                          passesFilter = all (\rd -> rd `elem` routeCodes) selectedFareRouteCodes
                       in (route.serviceTier, routeCodes, passesFilter)
                  )
                  possibleRoutes
          logDebug $ "filterAvailableBuses: route filter analysis = " <> show routeFilterResults
          let possibleServiceTiers = map (.serviceTier) $ filter (\route -> all (\rd -> rd `elem` map (.routeCode) route.availableRoutesInfo) selectedFareRouteCodes) possibleRoutes
          logDebug $ "filterAvailableBuses: final possibleServiceTiers = " <> show possibleServiceTiers
          let filteredFares = filter (\fare -> fare.vehicleServiceTier.serviceTierType `elem` possibleServiceTiers) fares
          logDebug $ "filterAvailableBuses: returning possibleServiceTiers = " <> show possibleServiceTiers <> ", filteredFares count = " <> show (length filteredFares)
          return ((Just possibleServiceTiers, filteredFares), Just possibleRoutes)
        _ -> return ((Nothing, fares), Nothing)

    selectMinFare :: [FRFSFare] -> Maybe FRFSFare
    selectMinFare [] = Nothing
    selectMinFare fares =
      Just $
        minimumBy
          ( \fare1 fare2 ->
              let fare1Amt = maybe 0.0 (.price.amount.getHighPrecMoney) (find (\category -> category.category == ADULT) fare1.categories)
                  fare2Amt = maybe 0.0 (.price.amount.getHighPrecMoney) (find (\category -> category.category == ADULT) fare2.categories)
               in compare fare1Amt fare2Amt
          )
          fares

    selectMaxFare :: [FRFSFare] -> Maybe FRFSFare
    selectMaxFare [] = Nothing
    selectMaxFare fares =
      Just $
        maximumBy
          ( \fare1 fare2 ->
              let fare1Amt = maybe 0.0 (.price.amount.getHighPrecMoney) (find (\category -> category.category == ADULT) fare1.categories)
                  fare2Amt = maybe 0.0 (.price.amount.getHighPrecMoney) (find (\category -> category.category == ADULT) fare2.categories)
               in compare fare1Amt fare2Amt
          )
          fares

getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => Id FRFSSearch -> DJourneyLeg.JourneyLeg -> [DJourneyLeg.JourneyLeg] -> m (Maybe JT.LegInfo)
getInfo searchId journeyLeg journeyLegs = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      legInfo <- JT.mkLegInfoFromFrfsBooking booking journeyLeg
      return (Just legInfo)
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      legInfo <- JT.mkLegInfoFromFrfsSearchRequest searchReq journeyLeg journeyLegs
      return (Just legInfo)

search :: JT.SearchRequestFlow m r c => Spec.VehicleCategory -> Id DPerson.Person -> Id DMerchant.Merchant -> Int -> Context.City -> DJourneyLeg.JourneyLeg -> Maybe (Id DRL.RecentLocation) -> Maybe Text -> Maybe Spec.ServiceTierType -> (Text -> m ()) -> m JT.SearchResponse
search vehicleCategory personId merchantId quantity city journeyLeg recentLocationId multimodalSearchRequestId mbServiceTier upsertJourneyLegAction = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show city)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromAgency (journeyLeg.agency <&> (.name)) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) DIBC.MULTIMODAL
  frfsRouteDetails <- getFrfsRouteDetails journeyLeg.routeDetails
  frfsSearchReq <- buildFRFSSearchReq frfsRouteDetails
  let mbFare = journeyLeg.estimatedMinFare <|> journeyLeg.estimatedMaxFare
  res <- FRFSTicketService.postFrfsSearchHandler (personId, merchantId) merchantOpCity integratedBPPConfig vehicleCategory frfsSearchReq frfsRouteDetails Nothing Nothing mbFare multimodalSearchRequestId upsertJourneyLegAction
  return $ JT.SearchResponse {id = res.searchId.getId}
  where
    buildFRFSSearchReq frfsRouteDetails = do
      fromStationCode <- ((journeyLeg.fromStopDetails >>= (.stopCode)) <|> ((journeyLeg.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "From station gtfsId not found")
      toStationCode <- ((journeyLeg.toStopDetails >>= (.stopCode)) <|> ((journeyLeg.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "To station gtfsId not found")
      let routeCode = listToMaybe frfsRouteDetails >>= (.routeCode)
          searchAsParentStops = Nothing
      return $ API.FRFSSearchAPIReq {..}

    getFrfsRouteDetails :: JT.SearchRequestFlow m r c => [RD.RouteDetails] -> m [FRFSRouteDetails]
    getFrfsRouteDetails routeDetails = do
      mapM
        ( \rd -> do
            startStationCode <- (rd.fromStopCode <|> (rd.fromStopGtfsId <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "From station gtfsId not found")
            endStationCode <- (rd.toStopCode <|> (rd.toStopGtfsId <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "To station gtfsId not found")
            routeCode <- (rd.routeGtfsId <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "Route gtfsId not found")
            return $ FRFSRouteDetails {routeCode = Just routeCode, serviceTier = mbServiceTier, ..}
        )
        routeDetails

confirm :: JT.ConfirmFlow m r c => Id DPerson.Person -> Id DMerchant.Merchant -> Maybe (Id FRFSQuote) -> Bool -> Bool -> Maybe APITypes.CrisSdkResponse -> Spec.VehicleCategory -> [API.FRFSCategorySelectionReq] -> Maybe Bool -> Maybe Bool -> m ()
confirm personId merchantId mbQuoteId bookLater bookingAllowed crisSdkResponse vehicleType categorySelectionReq isSingleMode mbEnableOffer = do
  when (not bookLater && bookingAllowed) $ do
    quoteId <- mbQuoteId & fromMaybeM (InvalidRequest "You can't confirm bus before getting the fare")
    quote <- QFRFSQuote.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
    integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
    case integratedBppConfig.providerConfig of
      DIBC.ONDC _ -> do
        merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
        merchantOperatingCity <- CQMOC.findById quote.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound quote.merchantOperatingCityId.getId)
        bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
        FRFSTicketService.select (processOnSelect integratedBppConfig) merchant merchantOperatingCity bapConfig quote categorySelectionReq isSingleMode mbEnableOffer
      _ -> do
        void $ FRFSTicketService.postFrfsQuoteV2ConfirmUtil (Just personId, merchantId) quote categorySelectionReq crisSdkResponse isSingleMode mbEnableOffer integratedBppConfig
  where
    processOnSelect :: (FRFSConfirmFlow m r) => DIBC.IntegratedBPPConfig -> DOnSelect -> Maybe Bool -> Maybe Bool -> m ()
    processOnSelect integratedBppConfig onSelectReq mbSingleMode enableOffer = do
      (merchant', quote', _) <- DOnSelect.validateRequest onSelectReq
      DOnSelect.onSelect onSelectReq merchant' quote' mbSingleMode enableOffer crisSdkResponse integratedBppConfig

cancel :: JT.CancelFlow m r c => Id FRFSSearch -> Spec.CancellationType -> m ()
cancel searchId cancellationType = do
  mbMetroBooking <- QTBooking.findBySearchId searchId
  whenJust mbMetroBooking $ \metroBooking -> do
    merchant <- CQM.findById metroBooking.merchantId >>= fromMaybeM (MerchantDoesNotExist metroBooking.merchantId.getId)
    merchantOperatingCity <- CQMOC.findById metroBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound metroBooking.merchantOperatingCityId.getId)
    bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory metroBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
    CallExternalBPP.cancel merchant merchantOperatingCity bapConfig cancellationType metroBooking

-- Helper functions for bus tracking, adapted from Lib.JourneyModule.Base
-- These functions are suffixed with CFRFS to avoid potential name clashes if Lib.JourneyModule.Base is also imported.

defaultBusTrackingConfigFRFS :: DomainRiderConfig.BusTrackingConfig
defaultBusTrackingConfigFRFS =
  DomainRiderConfig.BusTrackingConfig
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

nearbyBusKeyFRFS :: Text
nearbyBusKeyFRFS = "bus_locations"

vehicleMetaKey :: Text
vehicleMetaKey = "bus_metadata_v2"

topVehicleCandidatesKeyFRFS :: Text -> Text
topVehicleCandidatesKeyFRFS journeyLegId = "journeyLegTopVehicleCandidates:" <> journeyLegId

resultKeyFRFS :: Text -> Text
resultKeyFRFS journeyLegId = "journeyLegResult:" <> journeyLegId

isYetToReachStop :: Text -> FullBusData -> Bool
isYetToReachStop stopCode bus =
  case bus.busData.eta_data of
    Just etaList ->
      case find (\eta -> eta.stopCode == stopCode) etaList of
        Just _ -> True
        Nothing -> False
    Nothing -> False

processBusLegState ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) =>
  UTCTime ->
  Maybe DJourneyLeg.JourneyLeg ->
  Maybe Text ->
  [APITypes.RiderLocationReq] ->
  Id MerchantOperatingCity ->
  Maybe Station ->
  Maybe Station ->
  [FullBusData] ->
  HM.HashMap Text (HM.HashMap Text RouteStopMapping) ->
  JMStateTypes.TrackingStatus ->
  Bool ->
  m [JT.VehiclePosition]
processBusLegState
  now
  mbCurrentLegDetails
  routeCodeToUseForTrackVehicles
  riderLastPoints
  merchantOperatingCityId
  mbUserBoardingStation
  mbLegEndStation
  allBusDataForRoute
  routeStopMappings
  journeyLegTrackingStatus
  movementDetected = do
    logDebug $ "movementDetected: " <> show movementDetected <> " journeyLegTrackingStatus: " <> show journeyLegTrackingStatus
    if (isOngoingJourneyLeg journeyLegTrackingStatus) && movementDetected
      then do
        let filteredBusData = case (mbUserBoardingStation, mbLegEndStation) of
              (_, Just destStation) -> filter (isYetToReachStop destStation.code) allBusDataForRoute
              _ -> allBusDataForRoute
        case (mbCurrentLegDetails, routeCodeToUseForTrackVehicles, listToMaybe riderLastPoints) of
          (Just legDetails, Just rc, Just userPos) -> do
            riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
            let busTrackingConfig = fromMaybe defaultBusTrackingConfigFRFS riderConfig.busTrackingConfig
            nearbyBusesETA <- getNearbyBusesFRFS userPos.latLong riderConfig
            logDebug $ "nearbyBusesETA: " <> show nearbyBusesETA <> "for route_id: " <> show rc
            let scoresForBuses = scoreBusesByDistanceFRFS userPos busTrackingConfig nearbyBusesETA
            logDebug $ "scoresForBuses: " <> show scoresForBuses
            votingSystemFRFS scoresForBuses legDetails busTrackingConfig

            topCandidatesRaw <- Hedis.zRangeWithScores (topVehicleCandidatesKeyFRFS (legDetails.id.getId)) 0 (-1)
            logDebug $ "topCandidatesRaw: " <> show topCandidatesRaw
            let mbTopCandidateId = listToMaybe [TE.decodeUtf8 bs | (bs, _) <- topCandidatesRaw]
            logDebug $ "mbTopCandidateId: " <> show mbTopCandidateId
            case mbTopCandidateId of
              Just topCandVehId -> do
                let mbBestFullBusData = find (\bd -> bd.vehicleNumber == topCandVehId) filteredBusData
                logDebug $ "mbBestFullBusData: " <> show mbBestFullBusData
                case mbBestFullBusData of
                  Just bestFullBusData -> do
                    let bestBusData = bestFullBusData.busData
                    let routeStopMapping = HM.lookup bestBusData.route_id routeStopMappings
                    let upcomingStops =
                          if journeyLegTrackingStatus `elem` [JMStateTypes.Arriving, JMStateTypes.AlmostArrived, JMStateTypes.Arrived]
                            then getUpcomingStopsForBus routeStopMapping now mbUserBoardingStation bestBusData False -- Stops up to boarding for OnTheWay
                            else getUpcomingStopsForBus routeStopMapping now mbLegEndStation bestBusData True -- Stops to destination for Ongoing/Finishing/Completed
                    pure
                      [ JT.VehiclePosition
                          { position = Just $ LatLong bestBusData.latitude bestBusData.longitude,
                            vehicleId = topCandVehId,
                            route_state = bestBusData.route_state,
                            upcomingStops = upcomingStops
                          }
                      ]
                  Nothing -> do
                    logDebug "No best bus data available, returning empty list"
                    pure []
              Nothing -> do
                logDebug "No top candidate vehicle ID available, returning empty list"
                pure []
          _ -> do
            logDebug "No top candidate vehicle ID available, returning empty list"
            pure []
      else do
        if isOngoingJourneyLeg journeyLegTrackingStatus && not movementDetected
          then do
            logDebug $ "No current leg details available" <> show journeyLegTrackingStatus
            case mbCurrentLegDetails of
              Just legDetails -> do
                let changedBuses = fromMaybe [] legDetails.changedBusesInSequence
                logDebug $ "changedBuses: " <> show changedBuses
                if null changedBuses
                  then do
                    findfilteredBusData mbUserBoardingStation allBusDataForRoute
                  else findVehiclePositionFromSequence (reverse changedBuses)
              Nothing -> do
                logDebug "No current leg details available, returning empty list"
                pure []
          else do
            logDebug $ "Journey leg is not ongoing or movement is not detected, returning empty list" <> show journeyLegTrackingStatus
            if journeyLegTrackingStatus `elem` [JMStateTypes.InPlan, JMStateTypes.Arriving, JMStateTypes.AlmostArrived, JMStateTypes.Arrived]
              then do
                findfilteredBusData mbUserBoardingStation allBusDataForRoute
              else do
                logDebug "No filtered bus data available, returning empty list"
                pure []
    where
      findVehiclePositionFromSequence :: (MonadFlow m) => [Text] -> m [JT.VehiclePosition]
      findVehiclePositionFromSequence [] = pure []
      findVehiclePositionFromSequence (busNum : rest) = do
        logDebug $ "Looking for bus number: " <> show busNum
        case find (\bd -> bd.vehicleNumber == busNum) allBusDataForRoute of
          Just bestBusData -> do
            let routeStopMapping = HM.lookup bestBusData.busData.route_id routeStopMappings
            let upcomingStops = getUpcomingStopsForBus routeStopMapping now mbLegEndStation bestBusData.busData True
            logDebug $ "findVehiclePositionFromSequence upcomingStops: " <> show upcomingStops <> " " <> show bestBusData.busData.latitude <> " " <> show bestBusData.busData.longitude
            pure
              [ JT.VehiclePosition
                  { position = Just $ LatLong bestBusData.busData.latitude bestBusData.busData.longitude,
                    vehicleId = busNum,
                    route_state = bestBusData.busData.route_state,
                    upcomingStops = upcomingStops
                  }
              ]
          Nothing -> do
            logDebug $ "No bus data found for vehicle number: " <> show rest
            findVehiclePositionFromSequence rest
      findfilteredBusData :: (MonadFlow m) => Maybe Station -> [FullBusData] -> m [JT.VehiclePosition]
      findfilteredBusData mbBoardingStation allBusData = do
        let filteredBusData = case mbBoardingStation of
              Just boardingStation -> filter (isYetToReachStop boardingStation.code) allBusData
              Nothing -> allBusData
        let (confirmedHighBuses, ghostBuses) = partition (\a -> a.busData.route_state == Just CQMMB.ConfirmedHigh) filteredBusData
        logInfo $ "confirmedHighBuses: " <> show (length confirmedHighBuses) <> " ghostBuses: " <> show (length ghostBuses)
        pure $
          map
            ( \bd -> do
                let routeStopMapping = HM.lookup bd.busData.route_id routeStopMappings
                JT.VehiclePosition
                  { position = Just $ LatLong bd.busData.latitude bd.busData.longitude,
                    vehicleId = bd.vehicleNumber,
                    route_state = bd.busData.route_state,
                    upcomingStops = getUpcomingStopsForBus routeStopMapping now mbBoardingStation bd.busData False
                  }
            )
            confirmedHighBuses

getUpcomingStopsForBus ::
  Maybe (HM.HashMap Text RouteStopMapping) ->
  UTCTime -> -- Current time (`now`)
  Maybe Station -> -- The target station (e.g., boarding or destination)
  BusData -> -- The specific bus's data, containing `eta_data`
  Bool -> -- `True` if filtering from current time onwards, `False` otherwise (e.g., for OnTheWay, we might want all stops up to boarding)
  [JT.NextStopDetails]
getUpcomingStopsForBus mbRouteStopMapping now mbTargetStation busData filterFromCurrentTime =
  case (busData.eta_data, mbRouteStopMapping) of
    (Just etaData, Just routeStopMapping) ->
      let -- Filter stops up to the target station
          stopsUpToTarget :: [CQMMB.BusStopETA] = case mbTargetStation of
            Just targetStation -> fst $ foldl' (\(eta_data_acc, foundTarget) bs -> if not foundTarget then (bs : eta_data_acc, bs.stopCode == targetStation.code) else (eta_data_acc, True)) ([], False) etaData
            Nothing -> etaData

          -- Further filter from current time if required
          filteredStops =
            if filterFromCurrentTime
              then filter (\bs -> bs.arrivalTime > now) stopsUpToTarget
              else stopsUpToTarget

          -- Map BusStopETA to NextStopDetails
          toNextStopDetails bs =
            let mbStop = HM.lookup bs.stopCode routeStopMapping
             in case mbStop of
                  Just stop -> do
                    JT.NextStopDetails
                      { stopCode = bs.stopCode,
                        sequenceNumber = stop.sequenceNum,
                        travelTime = Just . nominalDiffTimeToSeconds $ diffUTCTime bs.arrivalTime now,
                        travelDistance = Nothing,
                        stopName = Just stop.stopName
                      }
                  Nothing ->
                    JT.NextStopDetails
                      { stopCode = bs.stopCode,
                        sequenceNumber = 0, -- THIS CASE SHOULD NEVER COME, IF ITS HAPPENEING SOMETHING IS OFF IN DATA
                        travelTime = Nothing,
                        travelDistance = Nothing,
                        stopName = Nothing
                      }
       in map toNextStopDetails filteredStops
    _ -> []

getNearbyBusesFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => LatLong -> DomainRiderConfig.RiderConfig -> m [BusDataWithRoutesInfo]
getNearbyBusesFRFS userPos' riderConfig = do
  let nearbyDriverSearchRadius :: Double = fromMaybe 0.5 riderConfig.nearbyDriverSearchRadius
  busesBS <- mapM (pure . decodeUtf8) =<< (CQMMB.withCrossAppRedisNew $ Hedis.geoSearch nearbyBusKeyFRFS (Hedis.FromLonLat userPos'.lon userPos'.lat) (Hedis.ByRadius nearbyDriverSearchRadius "km"))
  logDebug $ "getNearbyBusesFRFS: busesBS: " <> show busesBS
  buses <-
    if null busesBS
      then do
        logDebug $ "getNearbyBusesFRFS: No buses found in geo search, returning empty list"
        pure []
      else do
        logDebug $ "getNearbyBusesFRFS: Fetching bus metadata for " <> show (length busesBS) <> " buses"
        CQMMB.withCrossAppRedisNew $ Hedis.hmGet vehicleMetaKey busesBS
  logDebug $ "getNearbyBusesFRFS: buses: " <> show buses
  pure $ catMaybes buses

scoreByDistanceFRFS :: Double -> DomainRiderConfig.BusTrackingConfig -> Double
scoreByDistanceFRFS distance busTrackingConfig
  | distance <= busTrackingConfig.maxScoreDistanceInMeters = busTrackingConfig.maxScore
  | distance <= busTrackingConfig.goodScoreDistanceInMeters = busTrackingConfig.goodScore
  | distance <= busTrackingConfig.fairScoreDistanceInMeters = busTrackingConfig.fairScore
  | otherwise = 0

scoreBusesByDistanceFRFS :: APITypes.RiderLocationReq -> DomainRiderConfig.BusTrackingConfig -> [BusDataWithRoutesInfo] -> [(BusDataWithRoutesInfo, Double)]
scoreBusesByDistanceFRFS passengerLoc busTrackingConfig = map assignScore . filter isRecent
  where
    now = passengerLoc.currTime

    isRecent :: BusDataWithRoutesInfo -> Bool
    isRecent bus =
      let pingTime = posixSecondsToUTCTime (fromIntegral bus.timestamp)
          timeDiff = abs (Time.diffUTCTime now pingTime)
       in timeDiff < (realToFrac busTrackingConfig.thresholdSeconds)

    assignScore :: BusDataWithRoutesInfo -> (BusDataWithRoutesInfo, Double)
    assignScore bus =
      let busLoc = LatLong bus.latitude bus.longitude
          distanceMeters = distanceBetweenInMeters passengerLoc.latLong busLoc
          dist :: Double = realToFrac (highPrecMetersToMeters distanceMeters) -- Ensure conversion to simple meters
          score = scoreByDistanceFRFS dist busTrackingConfig
       in (bus, score)

isWorseThanThresholdFRFS :: Double -> Double -> Double -> Bool
isWorseThanThresholdFRFS candidateScore bestScore worseThreshold = candidateScore < (worseThreshold * bestScore)

addAllScoresFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(BusDataWithRoutesInfo, Double)] -> DJourneyLeg.JourneyLeg -> m ()
addAllScoresFRFS scoredBuses leg = do
  forM_ scoredBuses $ \(bus, points) -> do
    whenJust bus.vehicle_number $ \vehicle ->
      Hedis.zIncrBy (topVehicleCandidatesKeyFRFS leg.id.getId) (round points) vehicle

removeWorstMembersFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [Text] -> [(Text, Double)] -> DJourneyLeg.JourneyLeg -> Double -> DomainRiderConfig.BusTrackingConfig -> m ()
removeWorstMembersFRFS currentResultMembers allCandidates leg bestScore busTrackingConfig = do
  let membersToRemove =
        filter
          ( \m ->
              let scoreM = lookup m allCandidates
               in maybe False (\score -> isWorseThanThresholdFRFS score bestScore busTrackingConfig.thresholdFactor) scoreM
          )
          currentResultMembers
  forM_ membersToRemove $ \m -> Redis.srem (resultKeyFRFS leg.id.getId) [m]

addBetterMembersFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(Text, Double)] -> DJourneyLeg.JourneyLeg -> Double -> DomainRiderConfig.BusTrackingConfig -> m ()
addBetterMembersFRFS allCandidates leg bestScore busTrackingConfig = do
  forM_ allCandidates $ \(candidate, score) -> do
    unless (isWorseThanThresholdFRFS score bestScore busTrackingConfig.thresholdFactor) $
      Redis.sAddExp (resultKeyFRFS leg.id.getId) [candidate] 3600

votingSystemFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(BusDataWithRoutesInfo, Double)] -> DJourneyLeg.JourneyLeg -> DomainRiderConfig.BusTrackingConfig -> m ()
votingSystemFRFS scoredBuses leg busTrackingConfig = do
  addAllScoresFRFS scoredBuses leg
  Hedis.expire (topVehicleCandidatesKeyFRFS leg.id.getId) 3600
  bestCandidateResult <- Hedis.zrevrangeWithscores (topVehicleCandidatesKeyFRFS leg.id.getId) 0 0
  case bestCandidateResult of
    [] -> pure ()
    ((bestVehicleNumber, bestScore) : _) -> do
      let busesChanged = leg.changedBusesInSequence
      case busesChanged of
        Nothing -> QJourneyLeg.updateByPrimaryKey leg {DJourneyLeg.changedBusesInSequence = Just [bestVehicleNumber]}
        Just changedBusesInSequence ->
          case safeTail changedBusesInSequence of
            Nothing -> QJourneyLeg.updateByPrimaryKey leg {DJourneyLeg.changedBusesInSequence = Just [bestVehicleNumber]}
            Just x ->
              if x == bestVehicleNumber
                then pure ()
                else QJourneyLeg.updateByPrimaryKey leg {DJourneyLeg.changedBusesInSequence = Just $ changedBusesInSequence <> [bestVehicleNumber]}
      allCandidatesRaw <- Hedis.zRangeWithScores (topVehicleCandidatesKeyFRFS leg.id.getId) 0 (-1)
      let allCandidates = [(TE.decodeUtf8 bs, score) | (bs, score) <- allCandidatesRaw]
      currentResultMembers :: [Text] <- Hedis.sMembers (resultKeyFRFS leg.id.getId)
      Hedis.expire (resultKeyFRFS leg.id.getId) 3600
      removeWorstMembersFRFS currentResultMembers allCandidates leg bestScore busTrackingConfig
      addBetterMembersFRFS allCandidates leg bestScore busTrackingConfig

isOngoingJourneyLeg :: JMStateTypes.TrackingStatus -> Bool
isOngoingJourneyLeg legStatus = legStatus `elem` [JMStateTypes.Arriving, JMStateTypes.AlmostArrived, JMStateTypes.Arrived, JMStateTypes.Ongoing, JMStateTypes.Finishing]
