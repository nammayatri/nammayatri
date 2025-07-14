module Lib.JourneyLeg.Common.FRFS where

import qualified API.Types.UI.FRFSTicketService as API
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import Data.List (sortBy, sortOn)
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Types.FRFSQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RecentLocation as DRL
import qualified Domain.Types.RiderConfig as DomainRiderConfig
import Domain.Types.Station
import Domain.Types.StationType
import Domain.Types.Trip as DTrip
import Domain.Utils (safeLast)
import qualified EulerHS.Language as L
import EulerHS.Prelude (comparing)
import ExternalBPP.CallAPI as CallExternalBPP
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Maps.Types
import Kernel.External.MultiModal.Interface.Types (MultiModalLegGate)
import qualified Kernel.External.MultiModal.Interface.Types as EMTypes
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
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JT
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.CachedQueries.Merchant.MultiModalBus (BusData (..), BusDataWithoutETA (..))
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.RouteStopTimeTable as QRSTT
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.JourneyRouteDetails as QJRD
import Tools.Error

-- getState and other functions from the original file...

getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => DTrip.MultimodalTravelMode -> Id FRFSSearch -> [APITypes.RiderLocationReq] -> Bool -> Bool -> Maybe Text -> m JT.JourneyLegState
getState mode searchId riderLastPoints isLastCompleted movementDetected routeCodeForDetailedTracking = do
  mbBooking <- QTBooking.findBySearchId searchId
  now <- getCurrentTime
  let userPosition = (.latLong) <$> listToMaybe riderLastPoints
  case mbBooking of
    Just booking -> do
      integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
      case mode of
        DTrip.Bus -> do
          (statusChanged, newStatus) <- processOldStatus booking.journeyLegStatus booking.toStationCode integratedBppConfig isLastCompleted
          when statusChanged $ QTBooking.updateJourneyLegStatus (Just newStatus) booking.id
          journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")

          mbCurrentLegDetails <- QJourneyLeg.findByLegSearchId (Just searchId.getId)

          let routeCodeToUseForTrackVehicles = routeCodeForDetailedTracking <|> (mbCurrentLegDetails >>= (listToMaybe . (.routeDetails)) >>= (.gtfsId) <&> gtfsIdtoDomainCode)

          -- Fetch all bus data for the route using getRoutesBuses
          allBusDataForRoute <- case routeCodeToUseForTrackVehicles of
            Just rc -> map (.busData) . (.buses) <$> CQMMB.getRoutesBuses rc
            Nothing -> pure []

          -- Fetch user's boarding station and leg's end station details
          mbUserBoardingStation <- OTPRest.getStationByGtfsIdAndStopCode booking.fromStationCode integratedBppConfig
          mbLegEndStation <- OTPRest.getStationByGtfsIdAndStopCode booking.toStationCode integratedBppConfig

          let baseStateData =
                JT.JourneyLegStateData
                  { status = if newStatus == JPT.InPlan then JT.getFRFSLegStatusFromBooking booking else newStatus,
                    userPosition,
                    JT.vehiclePositions = [], -- Will be populated based on status
                    legOrder = journeyLegOrder,
                    subLegOrder = 1,
                    statusChanged,
                    mode
                  }

          vehiclePositionsToReturn <-
            processBusLegState
              now
              mbCurrentLegDetails
              routeCodeToUseForTrackVehicles
              riderLastPoints
              booking.merchantOperatingCityId
              mbUserBoardingStation
              mbLegEndStation
              allBusDataForRoute
              newStatus
              isLastCompleted
              movementDetected
              (flip QTBooking.updateJourneyLegStatus booking.id)

          let detailedStateData = baseStateData {JT.vehiclePositions = vehiclePositionsToReturn}

          finalStateData <-
            if detailedStateData.status `elem` [JPT.Finishing, JPT.Completed]
              then do
                case mbCurrentLegDetails of
                  Just legToUpdate -> do
                    when (isNothing legToUpdate.finalBoardedBusNumber) $ do
                      bestCandidateResult <- Hedis.zrevrangeWithscores (topVehicleCandidatesKeyFRFS (legToUpdate.id.getId)) 0 0
                      case bestCandidateResult of
                        [] -> pure ()
                        ((bestVehicleNumber, _) : _) -> do
                          QJourneyLeg.updateByPrimaryKey legToUpdate {DJourneyLeg.finalBoardedBusNumber = Just bestVehicleNumber}
                    pure detailedStateData
                  Nothing -> do
                    logError $ "CFRFS.getState: Could not find leg to update finalBoardedBusNumber for searchId: " <> searchId.getId
                    pure detailedStateData
              else pure detailedStateData

          return $ JT.Single finalStateData
        _ -> do
          -- Other modes (Metro, Subway, etc.)
          -- Note: The old getVehiclePosition is still used here.
          routeStatuses <- getStatusForMetroAndSubway booking.journeyRouteDetails booking.searchId integratedBppConfig isLastCompleted
          lastSubRoute <- safeLast routeStatuses & fromMaybeM (InternalError "New Status Not Found")
          let (_, lastStatusChanged, lastNewStatusFromSubway) = lastSubRoute
          when lastStatusChanged $ do
            QTBooking.updateJourneyLegStatus (Just lastNewStatusFromSubway) booking.id
          journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
          vehicleTrackingAndPositions <- do
            let findOngoingMetroOrSubway = find (\(_, _, currstatus) -> currstatus == JPT.Ongoing || currstatus == JPT.Finishing) routeStatuses
            case findOngoingMetroOrSubway of
              Just (_, _, currstatus) -> do
                getVehiclePosition booking.riderId booking.merchantId booking.merchantOperatingCityId currstatus userPosition booking.vehicleType booking.integratedBppConfigId (decodeFromText =<< booking.routeStationsJson)
              Nothing -> pure []
          let vehiclePositions =
                map
                  ( \(vehicleTracking, latLong) ->
                      JT.VehiclePosition
                        { position = latLong,
                          vehicleId = vehicleTracking.vehicleId,
                          upcomingStops =
                            vehicleTracking.upcomingStops
                              <&> ( \stopInfo ->
                                      JT.NextStopDetails
                                        { stopCode = stopInfo.stopCode,
                                          sequenceNumber = stopInfo.stopSeq,
                                          travelTime = (\t -> nominalDiffTimeToSeconds $ diffUTCTime t now) <$> stopInfo.estimatedTravelTime,
                                          travelDistance = stopInfo.travelDistance,
                                          stopName = Just stopInfo.stopName
                                        }
                                  )
                        }
                  )
                  vehicleTrackingAndPositions
          let journeyLegStates =
                [ JT.JourneyLegStateData
                    { status = if newStatus == JPT.InPlan then JT.getFRFSLegStatusFromBooking booking else newStatus,
                      userPosition,
                      JT.vehiclePositions = vehiclePositions,
                      legOrder = journeyLegOrder,
                      subLegOrder = fromMaybe 1 subRoute.subLegOrder,
                      statusChanged = changed,
                      mode
                    }
                  | (subRoute, changed, newStatus) <- routeStatuses
                ]
          return $ JT.Transit journeyLegStates
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity searchReq
      case mode of
        DTrip.Bus -> do
          (statusChanged, newStatus) <- processOldStatus searchReq.journeyLegStatus searchReq.toStationCode integratedBppConfig isLastCompleted
          when statusChanged $ QFRFSSearch.updateJourneyLegStatus (Just newStatus) searchReq.id
          journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest "JourneySearchData not found")
          mbCurrentLegDetails <- QJourneyLeg.findByLegSearchId (Just searchId.getId)

          let routeCodeToUseForTrackVehicles = routeCodeForDetailedTracking <|> (mbCurrentLegDetails >>= (listToMaybe . (.routeDetails)) >>= (.gtfsId) <&> gtfsIdtoDomainCode)

          -- Fetch all bus data for the route using getRoutesBuses
          allBusDataForRoute <- case routeCodeToUseForTrackVehicles of
            Just rc -> map (.busData) . (.buses) <$> CQMMB.getRoutesBuses rc
            Nothing -> pure []

          -- Fetch user's boarding station and leg's end station details
          mbUserBoardingStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.fromStationCode integratedBppConfig
          mbLegEndStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.toStationCode integratedBppConfig

          let baseStateData =
                JT.JourneyLegStateData
                  { status = newStatus,
                    userPosition,
                    JT.vehiclePositions = [], -- Will be populated based on status
                    legOrder = journeyLegInfo.journeyLegOrder,
                    subLegOrder = 1,
                    statusChanged,
                    mode
                  }

          vehiclePositionsToReturn <-
            processBusLegState
              now
              mbCurrentLegDetails
              routeCodeToUseForTrackVehicles
              riderLastPoints
              searchReq.merchantOperatingCityId
              mbUserBoardingStation
              mbLegEndStation
              allBusDataForRoute
              newStatus
              isLastCompleted
              movementDetected
              (flip QFRFSSearch.updateJourneyLegStatus searchId)

          let detailedStateData = baseStateData {JT.vehiclePositions = vehiclePositionsToReturn}

          finalStateData <-
            if detailedStateData.status `elem` [JPT.Finishing, JPT.Completed]
              then do
                case mbCurrentLegDetails of
                  Just legToUpdate -> do
                    when (isNothing legToUpdate.finalBoardedBusNumber) $ do
                      bestCandidateResult <- Hedis.zrevrangeWithscores (topVehicleCandidatesKeyFRFS (legToUpdate.id.getId)) 0 0
                      case bestCandidateResult of
                        [] -> pure ()
                        ((bestVehicleNumber, _) : _) -> do
                          QJourneyLeg.updateByPrimaryKey legToUpdate {DJourneyLeg.finalBoardedBusNumber = Just bestVehicleNumber}
                    pure detailedStateData
                  Nothing -> do
                    logError $ "CFRFS.getState: Could not find leg to update finalBoardedBusNumber for searchId: " <> searchId.getId
                    pure detailedStateData
              else pure detailedStateData

          return $ JT.Single finalStateData
        _ -> do
          -- Other modes (Metro, Subway, etc.)
          routeStatuses <- getStatusForMetroAndSubway searchReq.journeyRouteDetails searchReq.id integratedBppConfig isLastCompleted
          lastSubRoute <- safeLast routeStatuses & fromMaybeM (InternalError "New Status Not Found")
          let (_, lastStatusChanged, lastNewStatus) = lastSubRoute
          when lastStatusChanged $ QFRFSSearch.updateJourneyLegStatus (Just lastNewStatus) searchReq.id
          journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InternalError "JourneySearchData not found")
          vehicleTrackingAndPositions <- do
            let findOngoingMetroOrSubway = find (\(_, _, currstatus) -> currstatus == JPT.Ongoing || currstatus == JPT.Finishing) routeStatuses
            case findOngoingMetroOrSubway of
              Just (_, _, currstatus) -> do
                case journeyLegInfo.pricingId of
                  Just quoteId -> do
                    mbQuote <- QFRFSQuote.findById (Id quoteId)
                    case mbQuote of
                      Just quote -> do
                        getVehiclePosition quote.riderId quote.merchantId quote.merchantOperatingCityId currstatus userPosition quote.vehicleType quote.integratedBppConfigId (decodeFromText =<< quote.routeStationsJson)
                      Nothing -> do
                        logDebug $ "mbQuote not found."
                        pure []
                  Nothing -> do
                    logDebug $ "Quote not found."
                    pure []
              Nothing -> pure []
          let vehiclePositions =
                map
                  ( \(vehicleTracking, latLong) ->
                      JT.VehiclePosition
                        { position = latLong,
                          vehicleId = vehicleTracking.vehicleId,
                          upcomingStops =
                            vehicleTracking.upcomingStops
                              <&> ( \stopInfo ->
                                      JT.NextStopDetails
                                        { stopCode = stopInfo.stopCode,
                                          sequenceNumber = stopInfo.stopSeq,
                                          stopName = Just stopInfo.stopName,
                                          travelTime = (\t -> nominalDiffTimeToSeconds $ diffUTCTime t now) <$> stopInfo.estimatedTravelTime,
                                          travelDistance = stopInfo.travelDistance
                                        }
                                  )
                        }
                  )
                  vehicleTrackingAndPositions
          let journeyLegStates =
                [ JT.JourneyLegStateData
                    { status = newStatus,
                      userPosition,
                      JT.vehiclePositions = vehiclePositions,
                      legOrder = journeyLegInfo.journeyLegOrder,
                      subLegOrder = fromMaybe 1 subRoute.subLegOrder,
                      statusChanged = changed,
                      mode
                    }
                  | (subRoute, changed, newStatus) <- routeStatuses
                ]
          return $ JT.Transit journeyLegStates
  where
    getVehiclePosition :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DPerson.Person -> Id DMerchant.Merchant -> Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> JPT.JourneyLegStatus -> Maybe LatLong -> Spec.VehicleCategory -> Id DIBC.IntegratedBPPConfig -> Maybe [API.FRFSRouteStationsAPI] -> m [(VehicleTracking, LatLong)]
    getVehiclePosition riderId merchantId merchantOperatingCityId journeyStatus riderPosition vehicleType integratedBppConfigId mbRouteStations = do
      vehiclePositionResp <- try @_ @SomeException (getVehiclePosition' riderId merchantId merchantOperatingCityId journeyStatus riderPosition vehicleType integratedBppConfigId mbRouteStations)
      case vehiclePositionResp of
        Left err -> do
          logError $ "Error in getVehiclePosition: " <> show err
          return []
        Right vehiclePosition -> do
          return vehiclePosition

    getVehiclePosition' :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DPerson.Person -> Id DMerchant.Merchant -> Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> JPT.JourneyLegStatus -> Maybe LatLong -> Spec.VehicleCategory -> Id DIBC.IntegratedBPPConfig -> Maybe [API.FRFSRouteStationsAPI] -> m [(VehicleTracking, LatLong)]
    getVehiclePosition' riderId merchantId merchantOperatingCityId journeyStatus riderPosition vehicleType integratedBppConfigId = \case
      Just routesStations ->
        case listToMaybe routesStations of
          Just routeStations -> do
            case vehicleType of
              Spec.BUS -> do
                vehicleTracking <- trackVehicles riderId merchantId merchantOperatingCityId vehicleType routeStations.code DIBC.MULTIMODAL Nothing (Just integratedBppConfigId)
                if isUpcomingJourneyLeg journeyStatus
                  then do
                    let vehicleTrackingWithLatLong :: [(VehicleTracking, Double, Double)] =
                          mapMaybe
                            ( \vehicleTrack -> do
                                info <- vehicleTrack.vehicleInfo
                                (vehicleTrack,,)
                                  <$> info.latitude <*> info.longitude
                            )
                            vehicleTracking
                        mbStartStation = find (\station -> station.stationType == Just START) routeStations.stations
                        upcomingNearestVehicles =
                          sortBy
                            (comparing (\(vehicleTrack, _, _) -> maybe 0 (.sequenceNum) vehicleTrack.nextStop) <> comparing (\(vehicleTrack, _, _) -> vehicleTrack.nextStopTravelDistance))
                            $ filter
                              (\(vehicleTrack, _, _) -> maybe False (\startStation -> maybe False (\stationSequenceNum -> maybe False (\nextStop -> nextStop.sequenceNum <= stationSequenceNum) vehicleTrack.nextStop) startStation.sequenceNum) mbStartStation)
                              vehicleTrackingWithLatLong
                    pure ((\(vehicleTrack, lat, lon) -> (vehicleTrack, LatLong {..})) <$> upcomingNearestVehicles)
                  else
                    if isOngoingJourneyLeg journeyStatus
                      then do
                        case riderPosition of
                          Just riderLocation -> do
                            let vehicleTrackWithLatLong :: [(VehicleTracking, Double, Double)] =
                                  mapMaybe
                                    ( \vehicleTrack -> do
                                        info <- vehicleTrack.vehicleInfo
                                        (vehicleTrack,,)
                                          <$> info.latitude <*> info.longitude
                                    )
                                    vehicleTracking
                                nearestVehicleToUser = sortBy (comparing (\(_, lat, lon) -> distanceBetweenInMeters LatLong {..} riderLocation)) vehicleTrackWithLatLong
                            pure ((\(vehicleTrack, lat, lon) -> (vehicleTrack, LatLong {..})) <$> nearestVehicleToUser)
                          Nothing -> pure []
                      else pure []
              _ -> do
                case riderPosition of
                  Just riderLocation -> do
                    logDebug $ "Rider Location: " <> show riderLocation
                    vehicleTracking <- trackVehicles riderId merchantId merchantOperatingCityId vehicleType routeStations.code DIBC.MULTIMODAL Nothing (Just integratedBppConfigId)
                    pure ((\vehicleTrack -> (vehicleTrack, riderLocation)) <$> vehicleTracking)
                  Nothing -> do
                    logDebug $ "Rider Location not found."
                    pure []
          Nothing -> do
            logDebug $ "Routes Stations not found by listToMaybe."
            pure []
      Nothing -> do
        logDebug $ "Routes Stations not found."
        pure []

    isUpcomingJourneyLeg :: JPT.JourneyLegStatus -> Bool
    isUpcomingJourneyLeg legStatus = legStatus `elem` [JPT.Booked, JPT.InPlan, JPT.OnTheWay, JPT.Arriving, JPT.Arrived]

    processOldStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => Maybe JPT.JourneyLegStatus -> Text -> DIBC.IntegratedBPPConfig -> Bool -> m (Bool, JPT.JourneyLegStatus)
    processOldStatus mbOldStatus toStationCode integratedBppConfig isLastCompleted' = do
      mbToStation <- OTPRest.getStationByGtfsIdAndStopCode toStationCode integratedBppConfig
      let mbToLatLong = LatLong <$> (mbToStation >>= (.lat)) <*> (mbToStation >>= (.lon))
      let oldStatus = fromMaybe (if isLastCompleted' then JPT.OnTheWay else JPT.InPlan) mbOldStatus
      return $ maybe (False, oldStatus) (\latLong -> updateJourneyLegStatus mode riderLastPoints latLong oldStatus isLastCompleted') mbToLatLong

    getStatusForMetroAndSubway :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => [JPT.MultiModalJourneyRouteDetails] -> Id Domain.Types.FRFSSearch.FRFSSearch -> DIBC.IntegratedBPPConfig -> Bool -> m [(JPT.MultiModalJourneyRouteDetails, Bool, JPT.JourneyLegStatus)]
    getStatusForMetroAndSubway journeyRouteDetails searchId' integratedBppConfig isLastCompleted' = do
      let sortedSubRoutes = sortOn (.subLegOrder) journeyRouteDetails
      (_, (_, processedStatuses)) <-
        foldM
          ( \(isFirst, (prevStatus, acc)) subRoute -> do
              toStationCode <- subRoute.toStationCode & fromMaybeM (InternalError "Missing toStationCode")
              let newIsLastCompleted = if isFirst then prevStatus else False
              newStatus <- processOldStatus subRoute.journeyStatus toStationCode integratedBppConfig newIsLastCompleted
              if snd newStatus == JPT.Completed
                then pure (True, (newIsLastCompleted, newStatus : acc))
                else pure (False, (newIsLastCompleted, newStatus : acc))
          )
          (True, (isLastCompleted', []))
          sortedSubRoutes
      let processedStatuses' = reverse processedStatuses
      let newStatuses = zipWith (\subRoute (changed, newStatus) -> (subRoute, changed, newStatus)) sortedSubRoutes processedStatuses'
      forM_ newStatuses $ \(subRoute, statusChanged, newStatus) -> do
        when statusChanged $ do
          QJRD.updateJourneyStatus (Just newStatus) searchId' subRoute.subLegOrder
      pure newStatuses

getFare :: (CoreMetrics m, CacheFlow m r, EncFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, HasField "ltsHedisEnv" r Redis.HedisEnv, HasKafkaProducer r, HasShortDurationRetryCfg r c) => Id DPerson.Person -> DMerchant.Merchant -> MerchantOperatingCity -> Spec.VehicleCategory -> [FRFSRouteDetails] -> Maybe UTCTime -> Maybe Text -> m (Bool, Maybe JT.GetFareResponse)
getFare riderId merchant merchantOperatingCity vehicleCategory routeDetails mbFromArrivalTime agencyGtfsId = do
  let mbRouteDetail = mergeFFRFSRouteDetails routeDetails
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromAgency agencyGtfsId merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) DIBC.MULTIMODAL
  case (mbRouteDetail >>= (.routeCode), mbRouteDetail <&> (.startStationCode), mbRouteDetail <&> (.endStationCode)) of
    (Just routeCode, Just startStationCode, Just endStationCode) -> do
      QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory)
        >>= \case
          Just bapConfig -> do
            JMU.measureLatency (try @_ @SomeException $ Flow.getFares riderId merchant merchantOperatingCity integratedBPPConfig bapConfig routeCode startStationCode endStationCode vehicleCategory) ("getFares" <> show vehicleCategory <> " routeCode: " <> show routeCode <> " startStationCode: " <> show startStationCode <> " endStationCode: " <> show endStationCode)
              >>= \case
                Right (isFareMandatory, []) -> do
                  logError $ "Getting Empty Fares for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
                  return (isFareMandatory, Nothing)
                Right (isFareMandatory, fares) -> do
                  now <- getCurrentTime
                  let arrivalTime = fromMaybe now mbFromArrivalTime
                  L.setOptionLocal QRSTT.CalledForFare True
                  (possibleServiceTiers, availableFares) <- JMU.measureLatency (filterAvailableBuses arrivalTime startStationCode endStationCode integratedBPPConfig fares) ("filterAvailableBuses" <> show vehicleCategory <> " routeCode: " <> show routeCode <> " startStationCode: " <> show startStationCode <> " endStationCode: " <> show endStationCode)
                  L.setOptionLocal QRSTT.CalledForFare False
                  let mbMinFarePerRoute = selectMinFare availableFares
                  let mbMaxFarePerRoute = selectMaxFare availableFares
                  logDebug $ "all fares: " <> show fares <> "min fare: " <> show mbMinFarePerRoute <> "max fare: " <> show mbMaxFarePerRoute <> "possible service tiers: " <> show possibleServiceTiers <> "available fares: " <> show availableFares
                  case (mbMinFarePerRoute, mbMaxFarePerRoute) of
                    (Just minFare, Just maxFare) -> do
                      return (isFareMandatory, Just $ JT.GetFareResponse {serviceTypes = possibleServiceTiers, estimatedMinFare = minFare.price.amount, estimatedMaxFare = maxFare.price.amount})
                    _ -> do
                      logError $ "No Fare Found for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
                      return (isFareMandatory, Nothing)
                Left err -> do
                  logError $ "Exception Occured in Get Fare for Vehicle Category : " <> show vehicleCategory <> ", Error : " <> show err
                  return (True, Nothing)
          Nothing -> do
            logError $ "Did not get Beckn Config for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
            return (False, Nothing)
    _ -> do
      logError $ "No Route Details Found for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
      return (True, Nothing)
  where
    filterAvailableBuses :: (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, MonadFlow m, CacheFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv, HasKafkaProducer r, HasShortDurationRetryCfg r c) => UTCTime -> Text -> Text -> DIBC.IntegratedBPPConfig -> [FRFSFare] -> m (Maybe [Spec.ServiceTierType], [FRFSFare])
    filterAvailableBuses arrivalTime startStationCode endStationCode integratedBPPConfig fares = do
      case vehicleCategory of
        Spec.BUS -> do
          -- Above getFares function return fares for all types of buses (e.g. AC, Non-AC, Ordinary, etc.) but instead of showing all types of buses to user,
          -- Check for all possible buses available in next hour and just show fares for those buses to avoid confusion
          (_, possibleRoutes) <- JMU.findPossibleRoutes Nothing startStationCode endStationCode arrivalTime integratedBPPConfig merchant.id merchantOperatingCity.id Enums.BUS
          let possibleServiceTiers = map (.serviceTier) possibleRoutes
          return $ (Just possibleServiceTiers, filter (\fare -> fare.vehicleServiceTier.serviceTierType `elem` possibleServiceTiers) fares)
        _ -> return (Nothing, fares)

    selectMinFare :: [FRFSFare] -> Maybe FRFSFare
    selectMinFare [] = Nothing
    selectMinFare fares = Just $ minimumBy (\fare1 fare2 -> compare fare1.price.amount.getHighPrecMoney fare2.price.amount.getHighPrecMoney) fares

    selectMaxFare :: [FRFSFare] -> Maybe FRFSFare
    selectMaxFare [] = Nothing
    selectMaxFare fares = Just $ maximumBy (\fare1 fare2 -> compare fare1.price.amount.getHighPrecMoney fare2.price.amount.getHighPrecMoney) fares

getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => Id FRFSSearch -> Maybe HighPrecMoney -> Maybe Distance -> Maybe Seconds -> Maybe MultiModalLegGate -> Maybe MultiModalLegGate -> Bool -> m (Maybe JT.LegInfo)
getInfo searchId fallbackFare distance duration entrance exit ignoreOldSearchRequest = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      legInfo <- JT.mkLegInfoFromFrfsBooking booking distance duration entrance exit
      return (Just legInfo)
    Nothing ->
      if ignoreOldSearchRequest
        then return Nothing
        else do
          searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
          legInfo <- JT.mkLegInfoFromFrfsSearchRequest searchReq fallbackFare distance duration entrance exit
          return (Just legInfo)

search :: JT.SearchRequestFlow m r c => Spec.VehicleCategory -> Id DPerson.Person -> Id DMerchant.Merchant -> Int -> Context.City -> DJourneyLeg.JourneyLeg -> Maybe (Id DRL.RecentLocation) -> m JT.SearchResponse
search vehicleCategory personId merchantId quantity city journeyLeg recentLocationId = do
  let journeySearchData =
        JPT.JourneySearchData
          { journeyId = journeyLeg.journeyId.getId,
            journeyLegOrder = journeyLeg.sequenceNumber,
            agency = journeyLeg.agency <&> (.name),
            skipBooking = False,
            convenienceCost = 0,
            pricingId = Nothing,
            isDeleted = Just False,
            onSearchFailed = Nothing
          }
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show city)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromAgency journeySearchData.agency merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) DIBC.MULTIMODAL
  frfsSearchReq <- buildFRFSSearchReq (Just journeySearchData)
  frfsRouteDetails <- getFrfsRouteDetails journeyLeg.routeDetails
  journeyRouteDetails <- getJourneyRouteDetails journeyLeg.routeDetails integratedBPPConfig
  let mbFare = journeyLeg.estimatedMinFare <|> journeyLeg.estimatedMaxFare
  res <- FRFSTicketService.postFrfsSearchHandler (personId, merchantId) merchantOpCity integratedBPPConfig vehicleCategory frfsSearchReq frfsRouteDetails Nothing Nothing journeyRouteDetails mbFare
  return $ JT.SearchResponse {id = res.searchId.getId}
  where
    buildFRFSSearchReq journeySearchData = do
      fromStationCode <- ((journeyLeg.fromStopDetails >>= (.stopCode)) <|> ((journeyLeg.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "From station gtfsId not found")
      toStationCode <- ((journeyLeg.toStopDetails >>= (.stopCode)) <|> ((journeyLeg.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "To station gtfsId not found")
      let routeCode = Nothing
      return $ API.FRFSSearchAPIReq {..}

    getJourneyRouteDetails :: JT.SearchRequestFlow m r c => [EMTypes.MultiModalRouteDetails] -> DIBC.IntegratedBPPConfig -> m [JPT.MultiModalJourneyRouteDetails]
    getJourneyRouteDetails routeDetails integratedBPPConfig = do
      mapM transformJourneyRouteDetails routeDetails
      where
        transformJourneyRouteDetails :: JT.SearchRequestFlow m r c => EMTypes.MultiModalRouteDetails -> m JPT.MultiModalJourneyRouteDetails
        transformJourneyRouteDetails rd = do
          fromStationCode <- ((rd.fromStopDetails >>= (.stopCode)) <|> ((rd.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "From station gtfsId not found")
          toStationCode <- ((rd.toStopDetails >>= (.stopCode)) <|> ((rd.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "To station gtfsId not found")
          routeCode <- (rd.gtfsId <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "Route gtfsId not found")
          fromStation <- OTPRest.getStationByGtfsIdAndStopCode fromStationCode integratedBPPConfig
          toStation <- OTPRest.getStationByGtfsIdAndStopCode toStationCode integratedBPPConfig
          route <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode
          return
            JPT.MultiModalJourneyRouteDetails
              { platformNumber = rd.fromStopDetails >>= (.platformCode),
                lineColorCode = EMTypes.color rd,
                lineColor = EMTypes.shortName rd,
                alternateShortNames = EMTypes.alternateShortNames rd,
                frequency = Nothing,
                subLegOrder = Just (EMTypes.subLegOrder rd),
                journeyStatus = Nothing,
                routeLongName = EMTypes.longName rd,
                fromStationCode = fmap (.code) fromStation,
                toStationCode = fmap (.code) toStation,
                routeCode = fmap (.code) route
              }

    getFrfsRouteDetails :: JT.SearchRequestFlow m r c => [EMTypes.MultiModalRouteDetails] -> m [FRFSRouteDetails]
    getFrfsRouteDetails routeDetails = do
      mapM
        ( \rd -> do
            startStationCode <- ((rd.fromStopDetails >>= (.stopCode)) <|> ((rd.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "From station gtfsId not found")
            endStationCode <- ((rd.toStopDetails >>= (.stopCode)) <|> ((rd.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "To station gtfsId not found")
            routeCode <- (rd.gtfsId <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "Route gtfsId not found")
            return $ FRFSRouteDetails {routeCode = Just routeCode, ..}
        )
        routeDetails

confirm :: JT.ConfirmFlow m r c => Id DPerson.Person -> Id DMerchant.Merchant -> Maybe (Id FRFSQuote) -> Maybe Int -> Maybe Int -> Bool -> Bool -> Maybe APITypes.CrisSdkResponse -> Spec.VehicleCategory -> m ()
confirm personId merchantId mbQuoteId ticketQuantity childTicketQuantity skipBooking bookingAllowed crisSdkResponse vehicleType = do
  when (not skipBooking && bookingAllowed) $ do
    quoteId <- mbQuoteId & fromMaybeM (InvalidRequest "You can't confirm bus before getting the fare")
    quote <- QFRFSQuote.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
    if vehicleType == Spec.BUS
      then do
        merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
        merchantOperatingCity <- CQMOC.findById quote.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound quote.merchantOperatingCityId.getId)
        bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
        void $ CallExternalBPP.select merchant merchantOperatingCity bapConfig quote ticketQuantity childTicketQuantity
      else void $ FRFSTicketService.postFrfsQuoteV2ConfirmUtil (Just personId, merchantId) quoteId (API.FRFSQuoteConfirmReq {discounts = [], ticketQuantity = ticketQuantity, childTicketQuantity = childTicketQuantity}) crisSdkResponse

cancel :: JT.CancelFlow m r c => Id FRFSSearch -> Spec.CancellationType -> Bool -> m ()
cancel searchId cancellationType isSkipped = do
  mbMetroBooking <- QTBooking.findBySearchId searchId
  case mbMetroBooking of
    Just metroBooking -> do
      merchant <- CQM.findById metroBooking.merchantId >>= fromMaybeM (MerchantDoesNotExist metroBooking.merchantId.getId)
      merchantOperatingCity <- CQMOC.findById metroBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound metroBooking.merchantOperatingCityId.getId)
      bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory metroBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
      CallExternalBPP.cancel merchant merchantOperatingCity bapConfig cancellationType metroBooking
      if isSkipped then QTBooking.updateIsSkipped metroBooking.id (Just True) else QTBooking.updateIsCancelled metroBooking.id (Just True)
    Nothing -> do
      if isSkipped then QFRFSSearch.updateSkipBooking searchId (Just True) else QFRFSSearch.updateIsCancelled searchId (Just True)
  if isSkipped then QJourneyLeg.updateIsSkipped (Just True) (Just searchId.getId) else QJourneyLeg.updateIsDeleted (Just True) (Just searchId.getId)

isCancellable :: JT.CancelFlow m r c => Id FRFSSearch -> m JT.IsCancellableResponse
isCancellable searchId = do
  mbMetroBooking <- QTBooking.findBySearchId searchId
  case mbMetroBooking of
    Just metroBooking -> do
      frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow metroBooking.merchantOperatingCityId [] >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show metroBooking.merchantOperatingCityId)
      case metroBooking.journeyLegStatus of
        Just journeyLegStatus -> do
          let isBookingCancellable = journeyLegStatus `elem` JT.cannotCancelStatus
          case isBookingCancellable of
            True -> return $ JT.IsCancellableResponse {canCancel = False}
            False -> return $ JT.IsCancellableResponse {canCancel = frfsConfig.isCancellationAllowed}
        Nothing -> do
          return $ JT.IsCancellableResponse {canCancel = frfsConfig.isCancellationAllowed}
    Nothing -> do
      return $ JT.IsCancellableResponse {canCancel = True}

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
vehicleMetaKey = "bus_metadata"

topVehicleCandidatesKeyFRFS :: Text -> Text
topVehicleCandidatesKeyFRFS journeyLegId = "journeyLegTopVehicleCandidates:" <> journeyLegId

resultKeyFRFS :: Text -> Text
resultKeyFRFS journeyLegId = "journeyLegResult:" <> journeyLegId

isYetToReachStop :: Text -> BusData -> Bool
isYetToReachStop stopCode bus =
  case bus.eta_data of
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
  [BusData] ->
  JPT.JourneyLegStatus ->
  Bool ->
  Bool ->
  (Maybe JPT.JourneyLegStatus -> m ()) ->
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
  newStatus
  isLastCompleted
  movementDetected
  updateStatusFn =
    if (isLastCompleted || isOngoingJourneyLeg newStatus) && movementDetected
      then do
        let filteredBusData = case (mbUserBoardingStation, mbLegEndStation) of
              (_, Just destStation) -> filter (isYetToReachStop destStation.code) allBusDataForRoute
              _ -> allBusDataForRoute
        case (mbCurrentLegDetails, routeCodeToUseForTrackVehicles, listToMaybe riderLastPoints) of
          (Just legDetails, Just rc, Just userPos) -> do
            riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
            let busTrackingConfig = fromMaybe defaultBusTrackingConfigFRFS riderConfig.busTrackingConfig
            nearbyBusesETA <- getNearbyBusesFRFS userPos.latLong riderConfig
            let matchingBusesETA = filter (\x -> x.route_id == rc) nearbyBusesETA
            let nearbyFilteredBusesETA = filter (\bus -> highPrecMetersToMeters (distanceBetweenInMeters userPos.latLong (LatLong bus.latitude bus.longitude)) < 100) matchingBusesETA
            let scoresForBuses = scoreBusesByDistanceFRFS userPos busTrackingConfig nearbyFilteredBusesETA

            votingSystemFRFS scoresForBuses legDetails busTrackingConfig

            topCandidatesRaw <- Hedis.zRangeWithScores (topVehicleCandidatesKeyFRFS (legDetails.id.getId)) 0 (-1)
            let mbTopCandidateId = listToMaybe [TE.decodeUtf8 bs | (bs, _) <- topCandidatesRaw]

            case mbTopCandidateId of
              Just topCandVehId -> do
                let mbBestBusData = find (\bd -> bd.vehicle_number == Just topCandVehId) filteredBusData
                case mbBestBusData of
                  Just bestBusData -> do
                    let upcomingStops =
                          if newStatus `elem` [JPT.OnTheWay, JPT.Booked, JPT.Arriving]
                            then getUpcomingStopsForBus now mbUserBoardingStation bestBusData False -- Stops up to boarding for OnTheWay
                            else getUpcomingStopsForBus now mbLegEndStation bestBusData True -- Stops to destination for Ongoing/Finishing/Completed
                    when (newStatus `elem` [JPT.OnTheWay, JPT.Booked, JPT.Arriving]) $ do
                      updateStatusFn (Just JPT.Ongoing)

                    pure
                      [ JT.VehiclePosition
                          { position = LatLong bestBusData.latitude bestBusData.longitude,
                            vehicleId = topCandVehId,
                            upcomingStops = upcomingStops
                          }
                      ]
                  Nothing -> pure []
              Nothing -> pure []
          _ -> pure []
      else
        if isOngoingJourneyLeg newStatus && not movementDetected
          then case mbCurrentLegDetails of
            Just legDetails -> do
              let changedBuses = fromMaybe [] legDetails.changedBusesInSequence
              findVehiclePositionFromSequence (reverse changedBuses)
            Nothing -> pure []
          else
            if newStatus `elem` [JPT.InPlan, JPT.OnTheWay, JPT.Booked, JPT.Arriving]
              then do
                let filteredBusData = case mbUserBoardingStation of
                      Just boardingStation -> filter (isYetToReachStop boardingStation.code) allBusDataForRoute
                      Nothing -> allBusDataForRoute
                pure $
                  map
                    ( \bd ->
                        JT.VehiclePosition
                          { position = LatLong bd.latitude bd.longitude,
                            vehicleId = fromMaybe "UNKNOWN" bd.vehicle_number,
                            upcomingStops = getUpcomingStopsForBus now mbUserBoardingStation bd False
                          }
                    )
                    filteredBusData
              else pure []
    where
      findVehiclePositionFromSequence :: (MonadFlow m) => [Text] -> m [JT.VehiclePosition]
      findVehiclePositionFromSequence [] = pure []
      findVehiclePositionFromSequence (busNum : rest) = do
        case find (\bd -> bd.vehicle_number == Just busNum) allBusDataForRoute of
          Just bestBusData -> do
            let upcomingStops = getUpcomingStopsForBus now mbLegEndStation bestBusData True
            pure
              [ JT.VehiclePosition
                  { position = LatLong bestBusData.latitude bestBusData.longitude,
                    vehicleId = busNum,
                    upcomingStops = upcomingStops
                  }
              ]
          Nothing -> findVehiclePositionFromSequence rest

getUpcomingStopsForBus ::
  UTCTime -> -- Current time (`now`)
  Maybe Station -> -- The target station (e.g., boarding or destination)
  BusData -> -- The specific bus's data, containing `eta_data`
  Bool -> -- `True` if filtering from current time onwards, `False` otherwise (e.g., for OnTheWay, we might want all stops up to boarding)
  [JT.NextStopDetails]
getUpcomingStopsForBus now mbTargetStation busData filterFromCurrentTime =
  case busData.eta_data of
    Just etaData ->
      let -- Filter stops up to the target station
          stopsUpToTarget = case mbTargetStation of
            Just targetStation ->
              let mbTargetStopSeq = find (\bs -> bs.stopCode == targetStation.code) etaData <&> (.stopSeq)
               in case mbTargetStopSeq of
                    Just targetStopSeq -> filter (\bs -> bs.stopSeq <= targetStopSeq) etaData
                    Nothing -> etaData -- If target station not found, consider all stops
            Nothing -> etaData

          -- Further filter from current time if required
          filteredStops =
            if filterFromCurrentTime
              then filter (\bs -> bs.arrivalTime > now) stopsUpToTarget
              else stopsUpToTarget

          -- Map BusStopETA to NextStopDetails
          toNextStopDetails bs =
            JT.NextStopDetails
              { stopCode = bs.stopCode,
                sequenceNumber = bs.stopSeq,
                travelTime = Nothing,
                travelDistance = Nothing,
                stopName = Just bs.stopName
              }
       in map toNextStopDetails filteredStops
    Nothing -> []

getNearbyBusesFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => LatLong -> DomainRiderConfig.RiderConfig -> m [BusDataWithoutETA]
getNearbyBusesFRFS userPos' riderConfig = do
  let nearbyDriverSearchRadius :: Double = fromMaybe 0.5 riderConfig.nearbyDriverSearchRadius
  vehicleNumbers :: [Text] <-
    CQMMB.withCrossAppRedisNew $ -- Assuming CQMMB is available or can be made available
      Hedis.geoSearchDecoded nearbyBusKeyFRFS (Hedis.FromLonLat userPos'.lon userPos'.lat) (Hedis.ByRadius nearbyDriverSearchRadius "km")
  catMaybes <$> Hedis.hmGet vehicleMetaKey vehicleNumbers

scoreByDistanceFRFS :: Double -> DomainRiderConfig.BusTrackingConfig -> Double
scoreByDistanceFRFS distance busTrackingConfig
  | distance <= busTrackingConfig.maxScoreDistanceInMeters = busTrackingConfig.maxScore
  | distance <= busTrackingConfig.goodScoreDistanceInMeters = busTrackingConfig.goodScore
  | distance <= busTrackingConfig.fairScoreDistanceInMeters = busTrackingConfig.fairScore
  | otherwise = 0

scoreBusesByDistanceFRFS :: APITypes.RiderLocationReq -> DomainRiderConfig.BusTrackingConfig -> [BusDataWithoutETA] -> [(BusDataWithoutETA, Double)]
scoreBusesByDistanceFRFS passengerLoc busTrackingConfig = map assignScore . filter isRecent
  where
    now = passengerLoc.currTime

    isRecent :: BusDataWithoutETA -> Bool
    isRecent bus =
      let pingTime = posixSecondsToUTCTime (fromIntegral bus.timestamp)
          timeDiff = abs (Time.diffUTCTime now pingTime)
       in timeDiff < (realToFrac busTrackingConfig.thresholdSeconds)

    assignScore :: BusDataWithoutETA -> (BusDataWithoutETA, Double)
    assignScore bus =
      let busLoc = LatLong bus.latitude bus.longitude
          distanceMeters = distanceBetweenInMeters passengerLoc.latLong busLoc
          dist :: Double = realToFrac (highPrecMetersToMeters distanceMeters) -- Ensure conversion to simple meters
          score = scoreByDistanceFRFS dist busTrackingConfig
       in (bus, score)

isWorseThanThresholdFRFS :: Double -> Double -> Double -> Bool
isWorseThanThresholdFRFS candidateScore bestScore worseThreshold = candidateScore < (worseThreshold * bestScore)

addAllScoresFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(BusDataWithoutETA, Double)] -> DJourneyLeg.JourneyLeg -> m ()
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

votingSystemFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(BusDataWithoutETA, Double)] -> DJourneyLeg.JourneyLeg -> DomainRiderConfig.BusTrackingConfig -> m ()
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

isOngoingJourneyLeg :: JPT.JourneyLegStatus -> Bool
isOngoingJourneyLeg legStatus = legStatus `elem` [JPT.OnTheWay, JPT.Ongoing, JPT.Finishing]
