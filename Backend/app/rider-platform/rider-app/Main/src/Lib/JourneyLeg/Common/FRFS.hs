module Lib.JourneyLeg.Common.FRFS where

import API.Types.RiderPlatform.Management.Endpoints.FRFSTicket (FRFSStationAPI)
import qualified API.Types.UI.FRFSTicketService as API
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative
import Data.List (sortBy, sortOn)
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Action.UI.Location (makeLocationAPIEntity)
import Domain.Types.Booking.API as DBA
import Domain.Types.Extra.Booking
import Domain.Types.FRFSQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RecentLocation as DRL
import Domain.Types.Station
import Domain.Types.StationType
import Domain.Types.Trip as DTrip
import Domain.Utils (safeHead, safeLast)
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude (comparing, (+||), (||+))
import ExternalBPP.CallAPI as CallExternalBPP
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Maps.Types
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
import qualified Kernel.Types.TimeBound
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JT
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.RouteStopTimeTable as QRSTT
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.JourneyRouteDetails as QJRD
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.Station as QStation
import qualified Storage.Queries.Transformers.Booking as QTB
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg
import Tools.Error

getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => DTrip.MultimodalTravelMode -> Id FRFSSearch -> [APITypes.RiderLocationReq] -> Bool -> m JT.JourneyLegState
getState mode searchId riderLastPoints isLastCompleted = do
  mbBooking <- QTBooking.findBySearchId searchId
  let userPosition = (.latLong) <$> listToMaybe riderLastPoints
  case mbBooking of
    Just booking -> do
      case mode of
        DTrip.Bus -> do
          (statusChanged, newStatus) <- processOldStatus booking.journeyLegStatus booking.toStationId isLastCompleted
          when statusChanged $ QTBooking.updateJourneyLegStatus (Just newStatus) booking.id
          journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
          vehicleTrackingAndPositions <- do
            let mbRouteStations :: Maybe [API.FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
            getVehiclePosition booking.riderId booking.merchantId booking.merchantOperatingCityId newStatus userPosition booking.vehicleType mbRouteStations
          let vehiclePositions =
                map
                  ( \(vehicleTracking, latLong) ->
                      JT.VehiclePosition
                        { position = latLong,
                          vehicleId = vehicleTracking.vehicleId,
                          nextStop =
                            vehicleTracking.nextStop
                              <&> ( \nextStop ->
                                      JT.NextStopDetails
                                        { stopCode = nextStop.stopCode,
                                          sequenceNumber = nextStop.sequenceNum,
                                          travelTime = vehicleTracking.nextStopTravelTime,
                                          travelDistance = vehicleTracking.nextStopTravelDistance
                                        }
                                  )
                        }
                  )
                  vehicleTrackingAndPositions
          return $
            JT.Single $
              JT.JourneyLegStateData
                { status = if newStatus == JPT.InPlan then JT.getFRFSLegStatusFromBooking booking else newStatus,
                  userPosition,
                  vehiclePositions,
                  legOrder = journeyLegOrder,
                  subLegOrder = 1,
                  statusChanged,
                  mode
                }
        _ -> do
          routeStatuses <- getStatusForMetroAndSubway booking.journeyRouteDetails booking.searchId isLastCompleted
          lastSubRoute <- safeLast routeStatuses & fromMaybeM (InternalError "New Status Not Found")
          let (_, lastStatusChanged, lastNewStatus) = lastSubRoute
          when lastStatusChanged $ do
            QTBooking.updateJourneyLegStatus (Just lastNewStatus) booking.id
          journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
          vehicleTrackingAndPositions <- do
            let findOngoingMetroOrSubway = find (\(_, _, currstatus) -> currstatus == JPT.Ongoing || currstatus == JPT.Finishing) routeStatuses
            case findOngoingMetroOrSubway of
              Just (_, _, currstatus) -> do
                getVehiclePosition booking.riderId booking.merchantId booking.merchantOperatingCityId currstatus userPosition booking.vehicleType (decodeFromText =<< booking.routeStationsJson)
              Nothing -> pure []
          let vehiclePositions =
                map
                  ( \(vehicleTracking, latLong) ->
                      JT.VehiclePosition
                        { position = latLong,
                          vehicleId = vehicleTracking.vehicleId,
                          nextStop =
                            vehicleTracking.nextStop
                              <&> ( \nextStop ->
                                      JT.NextStopDetails
                                        { stopCode = nextStop.stopCode,
                                          sequenceNumber = nextStop.sequenceNum,
                                          travelTime = vehicleTracking.nextStopTravelTime,
                                          travelDistance = vehicleTracking.nextStopTravelDistance
                                        }
                                  )
                        }
                  )
                  vehicleTrackingAndPositions
          let journeyLegStates =
                [ JT.JourneyLegStateData
                    { status = if newStatus == JPT.InPlan then JT.getFRFSLegStatusFromBooking booking else newStatus,
                      userPosition,
                      vehiclePositions,
                      legOrder = journeyLegOrder,
                      subLegOrder = fromMaybe 1 subRoute.subLegOrder,
                      statusChanged = changed,
                      mode
                    }
                  | (subRoute, changed, newStatus) <- routeStatuses
                ]
          return $ JT.Transit journeyLegStates
    Nothing -> do
      case mode of
        DTrip.Bus -> do
          searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
          (statusChanged, newStatus) <- processOldStatus searchReq.journeyLegStatus searchReq.toStationId isLastCompleted
          when statusChanged $ QFRFSSearch.updateJourneyLegStatus (Just newStatus) searchReq.id
          journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest "JourneySearchData not found")
          vehicleTrackingAndPositions <- do
            case journeyLegInfo.pricingId of
              Just quoteId -> do
                mbQuote <- QFRFSQuote.findById (Id quoteId)
                case mbQuote of
                  Just quote -> do
                    let mbRouteStations :: Maybe [API.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
                    getVehiclePosition quote.riderId quote.merchantId quote.merchantOperatingCityId newStatus userPosition quote.vehicleType mbRouteStations
                  Nothing -> pure []
              Nothing -> pure []
          let vehiclePositions =
                map
                  ( \(vehicleTracking, latLong) ->
                      JT.VehiclePosition
                        { position = latLong,
                          vehicleId = vehicleTracking.vehicleId,
                          nextStop =
                            vehicleTracking.nextStop
                              <&> ( \nextStop ->
                                      JT.NextStopDetails
                                        { stopCode = nextStop.stopCode,
                                          sequenceNumber = nextStop.sequenceNum,
                                          travelTime = vehicleTracking.nextStopTravelTime,
                                          travelDistance = vehicleTracking.nextStopTravelDistance
                                        }
                                  )
                        }
                  )
                  vehicleTrackingAndPositions
          return $
            JT.Single $
              JT.JourneyLegStateData
                { status = newStatus,
                  userPosition,
                  vehiclePositions,
                  legOrder = journeyLegInfo.journeyLegOrder,
                  subLegOrder = 1,
                  statusChanged,
                  mode
                }
        _ -> do
          searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
          routeStatuses <- getStatusForMetroAndSubway searchReq.journeyRouteDetails searchReq.id isLastCompleted
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
                        let mbRouteStations :: Maybe [API.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
                        getVehiclePosition quote.riderId quote.merchantId quote.merchantOperatingCityId currstatus userPosition quote.vehicleType mbRouteStations
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
                          nextStop =
                            vehicleTracking.nextStop
                              <&> ( \nextStop ->
                                      JT.NextStopDetails
                                        { stopCode = nextStop.stopCode,
                                          sequenceNumber = nextStop.sequenceNum,
                                          travelTime = vehicleTracking.nextStopTravelTime,
                                          travelDistance = vehicleTracking.nextStopTravelDistance
                                        }
                                  )
                        }
                  )
                  vehicleTrackingAndPositions
          let journeyLegStates =
                [ JT.JourneyLegStateData
                    { status = newStatus,
                      userPosition,
                      vehiclePositions,
                      legOrder = journeyLegInfo.journeyLegOrder,
                      subLegOrder = fromMaybe 1 subRoute.subLegOrder,
                      statusChanged = changed,
                      mode
                    }
                  | (subRoute, changed, newStatus) <- routeStatuses
                ]
          return $ JT.Transit journeyLegStates
  where
    getVehiclePosition :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DPerson.Person -> Id DMerchant.Merchant -> Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> JPT.JourneyLegStatus -> Maybe LatLong -> Spec.VehicleCategory -> Maybe [API.FRFSRouteStationsAPI] -> m [(VehicleTracking, LatLong)]
    getVehiclePosition riderId merchantId merchantOperatingCityId journeyStatus riderPosition vehicleType mbRouteStations = do
      vehiclePositionResp <- try @_ @SomeException (getVehiclePosition' riderId merchantId merchantOperatingCityId journeyStatus riderPosition vehicleType mbRouteStations)
      case vehiclePositionResp of
        Left err -> do
          logError $ "Error in getVehiclePosition: " <> show err
          return []
        Right vehiclePosition -> do
          return vehiclePosition

    getVehiclePosition' :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DPerson.Person -> Id DMerchant.Merchant -> Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> JPT.JourneyLegStatus -> Maybe LatLong -> Spec.VehicleCategory -> Maybe [API.FRFSRouteStationsAPI] -> m [(VehicleTracking, LatLong)]
    getVehiclePosition' riderId merchantId merchantOperatingCityId journeyStatus riderPosition vehicleType = \case
      Just routesStations ->
        case listToMaybe routesStations of
          Just routeStations -> do
            case vehicleType of
              Spec.BUS -> do
                vehicleTracking <- trackVehicles riderId merchantId merchantOperatingCityId vehicleType routeStations.code DIBC.MULTIMODAL Nothing
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
                    vehicleTracking <- trackVehicles riderId merchantId merchantOperatingCityId vehicleType routeStations.code DIBC.MULTIMODAL riderPosition
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

    isOngoingJourneyLeg :: JPT.JourneyLegStatus -> Bool
    isOngoingJourneyLeg legStatus = legStatus `elem` [JPT.Ongoing]

    isUpcomingJourneyLeg :: JPT.JourneyLegStatus -> Bool
    isUpcomingJourneyLeg legStatus = legStatus `elem` [JPT.InPlan]

    processOldStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe JPT.JourneyLegStatus -> Id Station -> Bool -> m (Bool, JPT.JourneyLegStatus)
    processOldStatus mbOldStatus toStationId isLastCompleted' = do
      mbToStation <- QStation.findById toStationId
      let mbToLatLong = LatLong <$> (mbToStation >>= (.lat)) <*> (mbToStation >>= (.lon))
      let oldStatus = fromMaybe (if isLastCompleted' then JPT.Ongoing else JPT.InPlan) mbOldStatus
      return $ maybe (False, oldStatus) (\latLong -> updateJourneyLegStatus mode riderLastPoints latLong oldStatus isLastCompleted') mbToLatLong

    getStatusForMetroAndSubway :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => [JPT.MultiModalJourneyRouteDetails] -> Id Domain.Types.FRFSSearch.FRFSSearch -> Bool -> m [(JPT.MultiModalJourneyRouteDetails, Bool, JPT.JourneyLegStatus)]
    getStatusForMetroAndSubway journeyRouteDetails searchId' isLastCompleted' = do
      let sortedSubRoutes = sortOn (.subLegOrder) journeyRouteDetails
      (_, (_, processedStatuses)) <-
        foldM
          ( \(isFirst, (prevStatus, acc)) subRoute -> do
              toStationId <- subRoute.toStationId & fromMaybeM (InternalError "Missing toStationId")
              let newIsLastCompleted = if isFirst then prevStatus else False
              newStatus <- processOldStatus subRoute.journeyStatus toStationId newIsLastCompleted
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

getFare :: (CoreMetrics m, CacheFlow m r, EncFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv, HasKafkaProducer r) => Id DPerson.Person -> DMerchant.Merchant -> MerchantOperatingCity -> Spec.VehicleCategory -> [FRFSRouteDetails] -> Maybe UTCTime -> m (Maybe JT.GetFareResponse)
getFare riderId merchant merchantOperatingCity vehicleCategory routeDetails mbFromArrivalTime = do
  let mbRouteDetail = mergeFFRFSRouteDetails routeDetails
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) DIBC.MULTIMODAL >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleCategory ||+ "Platform Type:" +|| DIBC.MULTIMODAL ||+ "")
  case (mbRouteDetail >>= (.routeCode), mbRouteDetail <&> (.startStationCode), mbRouteDetail <&> (.endStationCode)) of
    (Just routeCode, Just startStationCode, Just endStationCode) -> do
      QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory)
        >>= \case
          Just bapConfig -> do
            try @_ @SomeException (Flow.getFares riderId merchant merchantOperatingCity integratedBPPConfig bapConfig routeCode startStationCode endStationCode vehicleCategory)
              >>= \case
                Right [] -> do
                  logError $ "Getting Empty Fares for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
                  return Nothing
                Right fares -> do
                  now <- getCurrentTime
                  let arrivalTime = fromMaybe now mbFromArrivalTime
                  L.setOptionLocal QRSTT.CalledForFare True
                  (possibleServiceTiers, availableFares) <- filterAvailableBuses arrivalTime startStationCode endStationCode integratedBPPConfig.id fares
                  L.setOptionLocal QRSTT.CalledForFare False
                  let mbMinFarePerRoute = selectMinFare availableFares
                  let mbMaxFarePerRoute = selectMaxFare availableFares
                  logDebug $ "all fares: " <> show fares <> "min fare: " <> show mbMinFarePerRoute <> "max fare: " <> show mbMaxFarePerRoute <> "possible service tiers: " <> show possibleServiceTiers <> "available fares: " <> show availableFares
                  case (mbMinFarePerRoute, mbMaxFarePerRoute) of
                    (Just minFare, Just maxFare) -> do
                      return (Just $ JT.GetFareResponse {serviceTypes = possibleServiceTiers, estimatedMinFare = minFare.price.amount, estimatedMaxFare = maxFare.price.amount})
                    _ -> do
                      logError $ "No Fare Found for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
                      return Nothing
                Left err -> do
                  logError $ "Exception Occured in Get Fare for Vehicle Category : " <> show vehicleCategory <> ", Error : " <> show err
                  return Nothing
          Nothing -> do
            logError $ "Did not get Beckn Config for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
            return Nothing
    _ -> do
      logError $ "No Route Details Found for Vehicle Category : " <> show vehicleCategory <> "for riderId: " <> show riderId
      return Nothing
  where
    filterAvailableBuses :: (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, MonadFlow m, CacheFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv, HasKafkaProducer r) => UTCTime -> Text -> Text -> Id DIBC.IntegratedBPPConfig -> [FRFSFare] -> m (Maybe [Spec.ServiceTierType], [FRFSFare])
    filterAvailableBuses arrivalTime startStationCode endStationCode integratedBPPConfigId fares = do
      case vehicleCategory of
        Spec.BUS -> do
          -- Above getFares function return fares for all types of buses (e.g. AC, Non-AC, Ordinary, etc.) but instead of showing all types of buses to user,
          -- Check for all possible buses available in next hour and just show fares for those buses to avoid confusion
          (_, possibleRoutes) <- JMU.findPossibleRoutes Nothing startStationCode endStationCode arrivalTime integratedBPPConfigId merchant.id merchantOperatingCity.id Enums.BUS
          let possibleServiceTiers = map (.serviceTier) possibleRoutes
          return $ (Just possibleServiceTiers, filter (\fare -> fare.vehicleServiceTier.serviceTierType `elem` possibleServiceTiers) fares)
        _ -> return (Nothing, fares)

    selectMinFare :: [FRFSFare] -> Maybe FRFSFare
    selectMinFare [] = Nothing
    selectMinFare fares = Just $ minimumBy (\fare1 fare2 -> compare fare1.price.amount.getHighPrecMoney fare2.price.amount.getHighPrecMoney) fares

    selectMaxFare :: [FRFSFare] -> Maybe FRFSFare
    selectMaxFare [] = Nothing
    selectMaxFare fares = Just $ maximumBy (\fare1 fare2 -> compare fare1.price.amount.getHighPrecMoney fare2.price.amount.getHighPrecMoney) fares

getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id FRFSSearch -> Maybe HighPrecMoney -> Maybe Distance -> Maybe Seconds -> m JT.LegInfo
getInfo searchId fallbackFare distance duration = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      JT.mkLegInfoFromFrfsBooking booking distance duration
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      JT.mkLegInfoFromFrfsSearchRequest searchReq fallbackFare distance duration

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
            isDeleted = Just False
          }
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show city)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) DIBC.MULTIMODAL
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleCategory ||+ "Platform Type:" +|| DIBC.MULTIMODAL ||+ "")
  frfsSearchReq <- buildFRFSSearchReq (Just journeySearchData) merchantOpCity integratedBPPConfig
  frfsRouteDetails <- getFrfsRouteDetails journeyLeg.routeDetails
  journeyRouteDetails <- getJourneyRouteDetails journeyLeg.routeDetails merchantOpCity integratedBPPConfig
  res <- FRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just city) vehicleCategory frfsSearchReq frfsRouteDetails Nothing Nothing journeyRouteDetails DIBC.MULTIMODAL
  return $ JT.SearchResponse {id = res.searchId.getId}
  where
    buildFRFSSearchReq journeySearchData merchantOpCity integratedBPPConfig = do
      fromStationCode <- ((journeyLeg.fromStopDetails >>= (.stopCode)) <|> ((journeyLeg.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "From station gtfsId not found")
      toStationCode <- ((journeyLeg.toStopDetails >>= (.stopCode)) <|> ((journeyLeg.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "To station gtfsId not found")
      _ <- createStationIfRequired (journeyLeg.fromStopDetails >>= (.name)) fromStationCode journeyLeg.startLocation.latitude journeyLeg.startLocation.longitude merchantOpCity integratedBPPConfig
      _ <- createStationIfRequired (journeyLeg.toStopDetails >>= (.name)) toStationCode journeyLeg.endLocation.latitude journeyLeg.endLocation.longitude merchantOpCity integratedBPPConfig
      let routeCode = Nothing
      return $ API.FRFSSearchAPIReq {..}

    createStationIfRequired :: JT.SearchRequestFlow m r c => Maybe Text -> Text -> Double -> Double -> MerchantOperatingCity -> DIBC.IntegratedBPPConfig -> m (Maybe Station)
    createStationIfRequired name code lat lon merchantOpCity integratedBPPConfig = do
      mbStation <- QStation.findByStationCode code integratedBPPConfig.id
      case mbStation of
        Just station -> return (Just station)
        Nothing -> do
          mbNewStation <- createStation name code lat lon merchantOpCity.id integratedBPPConfig
          whenJust mbNewStation $ \station -> QStation.create station
          return mbNewStation

    createStation :: JT.SearchRequestFlow m r c => Maybe Text -> Text -> Double -> Double -> Id MerchantOperatingCity -> DIBC.IntegratedBPPConfig -> m (Maybe Station)
    createStation Nothing _ _ _ _ _ = return Nothing
    createStation (Just name) code lat lon merchantOpCityId integratedBPPConfig = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        Just $
          Station
            { id = newId,
              vehicleType = vehicleCategory,
              name = name,
              possibleTypes = Nothing,
              code = code,
              lat = Just lat,
              lon = Just lon,
              address = Nothing,
              merchantId = merchantId,
              timeBounds = Kernel.Types.TimeBound.Unbounded,
              merchantOperatingCityId = merchantOpCityId,
              integratedBppConfigId = integratedBPPConfig.id,
              suggestedDestinations = Nothing,
              regionalName = Nothing,
              hindiName = Nothing,
              createdAt = now,
              updatedAt = now
            }

    getJourneyRouteDetails :: JT.SearchRequestFlow m r c => [EMTypes.MultiModalRouteDetails] -> MerchantOperatingCity -> DIBC.IntegratedBPPConfig -> m [JPT.MultiModalJourneyRouteDetails]
    getJourneyRouteDetails routeDetails merchantOpCity integratedBPPConfig = do
      mapM transformJourneyRouteDetails routeDetails
      where
        transformJourneyRouteDetails :: JT.SearchRequestFlow m r c => EMTypes.MultiModalRouteDetails -> m JPT.MultiModalJourneyRouteDetails
        transformJourneyRouteDetails rd = do
          fromStationCode <- ((rd.fromStopDetails >>= (.stopCode)) <|> ((rd.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "From station gtfsId not found")
          toStationCode <- ((rd.toStopDetails >>= (.stopCode)) <|> ((rd.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InvalidRequest "To station gtfsId not found")
          routeCode <- (rd.gtfsId <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "Route gtfsId not found")
          fromStation <- createStationIfRequired (rd.fromStopDetails >>= (.name)) fromStationCode rd.startLocation.latLng.latitude rd.startLocation.latLng.longitude merchantOpCity integratedBPPConfig
          toStation <- createStationIfRequired (rd.toStopDetails >>= (.name)) toStationCode rd.endLocation.latLng.latitude rd.endLocation.latLng.longitude merchantOpCity integratedBPPConfig
          route <- QRoute.findByRouteCode routeCode integratedBPPConfig.id
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
                fromStationId = fmap (.id) fromStation,
                toStationId = fmap (.id) toStation,
                routeId = fmap (.id) route
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

confirm :: JT.ConfirmFlow m r c => Id DPerson.Person -> Id DMerchant.Merchant -> Id FRFSSearch -> Maybe (Id FRFSQuote) -> Maybe Int -> Maybe Int -> Bool -> Bool -> Maybe APITypes.CrisSdkResponse -> m ()
confirm personId merchantId searchId mbQuoteId ticketQuantity childTicketQuantity skipBooking bookingAllowed crisSdkResponse = do
  mbBooking <- QTBooking.findBySearchId searchId -- if booking already there no need to confirm again
  when (not skipBooking && bookingAllowed && isNothing mbBooking) $ do
    quoteId <- mbQuoteId & fromMaybeM (InvalidRequest "You can't confirm bus before getting the fare")
    void $ FRFSTicketService.postFrfsQuoteConfirmPlatformType (Just personId, merchantId) quoteId ticketQuantity childTicketQuantity DIBC.MULTIMODAL crisSdkResponse

cancel :: JT.CancelFlow m r c => Id FRFSSearch -> Spec.CancellationType -> Bool -> m ()
cancel searchId cancellationType isSkipped = do
  mbMetroBooking <- QTBooking.findBySearchId searchId
  case mbMetroBooking of
    Just metroBooking -> do
      merchant <- CQM.findById metroBooking.merchantId >>= fromMaybeM (MerchantDoesNotExist metroBooking.merchantId.getId)
      merchantOperatingCity <- CQMOC.findById metroBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound metroBooking.merchantOperatingCityId.getId)
      bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory metroBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
      CallExternalBPP.cancel merchant merchantOperatingCity bapConfig cancellationType metroBooking DIBC.MULTIMODAL
      if isSkipped then QTBooking.updateIsSkipped metroBooking.id (Just True) else QTBooking.updateIsCancelled metroBooking.id (Just True)
    Nothing -> do
      if isSkipped then QFRFSSearch.updateSkipBooking searchId (Just True) else QFRFSSearch.updateIsCancelled searchId (Just True)
  if isSkipped then QJourneyLeg.updateIsSkipped (Just True) (Just searchId.getId) else QJourneyLeg.updateIsDeleted (Just True) (Just searchId.getId)

isCancellable :: JT.CancelFlow m r c => Id FRFSSearch -> m JT.IsCancellableResponse
isCancellable searchId = do
  mbMetroBooking <- QTBooking.findBySearchId searchId
  case mbMetroBooking of
    Just metroBooking -> do
      routeInfo <- getRouteByStationIdsAndIntegratedBPPConfigId metroBooking.fromStationId metroBooking.toStationId metroBooking.integratedBppConfigId
      let routeId = routeInfo <&> (.route.id)
      frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityIdAndRouteId metroBooking.merchantOperatingCityId routeId >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show metroBooking.merchantOperatingCityId)
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

getLegSourceAndDestination :: Maybe DJourneyLeg.JourneyLeg -> Bool -> Flow DBA.JourneyLocation
getLegSourceAndDestination Nothing _ = pure Null
getLegSourceAndDestination (Just leg) getSource = do
  let legSearchId = Id <$> leg.legSearchId
  case legSearchId of
    Nothing -> pure Null
    Just searchId -> case leg.mode of
      DTrip.Walk -> getWalkLocation searchId
      DTrip.Taxi -> getTaxiLocation searchId
      _ -> getPublicTransportLocation searchId
  where
    getLegLocation :: a -> a -> Flow a
    getLegLocation fromLocation toLocation = pure $ if getSource then fromLocation else toLocation

    getWalkLocation :: Id Text -> Flow DBA.JourneyLocation
    getWalkLocation searchId = do
      legData <- QWalkLeg.findById (cast searchId) >>= fromMaybeM (InvalidRequest "WalkLeg Data not found")
      toLocation' <- legData.toLocation & fromMaybeM (InvalidRequest "To location not found")
      location <- getLegLocation legData.fromLocation toLocation'
      pure $ DBA.Taxi $ makeLocationAPIEntity location

    getTaxiLocation :: Id Text -> Flow DBA.JourneyLocation
    getTaxiLocation searchId = do
      mbBooking <- QBooking.findByTransactionIdAndStatus searchId.getId (activeBookingStatus <> [COMPLETED])
      case mbBooking of
        Just booking -> do
          toLocation <- QTB.getToLocation booking.bookingDetails & fromMaybeM (InvalidRequest "To Location not found")
          location <- getLegLocation booking.fromLocation toLocation
          pure $ DBA.Taxi $ makeLocationAPIEntity location
        Nothing -> do
          searchReq <- QSearchRequest.findById (cast searchId) >>= fromMaybeM (SearchRequestNotFound searchId.getId)
          toLocation <- searchReq.toLocation & fromMaybeM (InvalidRequest "To location not found")
          location <- getLegLocation searchReq.fromLocation toLocation
          pure $ DBA.Taxi $ makeLocationAPIEntity location

    getPublicTransportLocation :: Id Text -> Flow DBA.JourneyLocation
    getPublicTransportLocation searchId = do
      mbBooking <- QTBooking.findBySearchId (cast searchId)
      case mbBooking of
        Just booking -> getFrfsLocation booking.vehicleType booking.journeyRouteDetails
        Nothing -> do
          searchReq <- QFRFSSearch.findById (cast searchId) >>= fromMaybeM (SearchRequestNotFound searchId.getId)
          getFrfsLocation searchReq.vehicleType searchReq.journeyRouteDetails

    getFrfsLocation :: Spec.VehicleCategory -> [JPT.MultiModalJourneyRouteDetails] -> Flow DBA.JourneyLocation
    getFrfsLocation Spec.BUS journeyRouteDetails = do
      journeyRouteDetail <- listToMaybe journeyRouteDetails & fromMaybeM (InternalError "Journey Route Detail not found")
      fromStation <- getStation journeyRouteDetail.fromStationId
      toStation <- getStation journeyRouteDetail.toStationId
      location <- getLegLocation fromStation toStation
      pure $ DBA.Frfs $ JT.stationToStationAPI location
    getFrfsLocation Spec.METRO journeyRouteDetails = do
      metroLegRouteInfo <- JT.getMetroLegRouteInfo journeyRouteDetails
      getFrfsSourceAndDestinationHelper (map Right metroLegRouteInfo)
    getFrfsLocation Spec.SUBWAY journeyRouteDetails = do
      subwayLegRouteInfo <- JT.getSubwayLegRouteInfo journeyRouteDetails
      getFrfsSourceAndDestinationHelper (map Left subwayLegRouteInfo)

    getFrfsSourceAndDestinationHelper :: [Either JT.SubwayLegRouteInfo JT.MetroLegRouteInfo] -> Flow DBA.JourneyLocation
    getFrfsSourceAndDestinationHelper routeInfo = do
      let (source, destination) = getFrfsSourceAndDestination routeInfo
      case getSource of
        True -> pure $ either DBA.Frfs (const Null) source
        False -> pure $ either DBA.Frfs (const Null) destination

    getStation :: Maybe (Kernel.Types.Id.Id Domain.Types.Station.Station) -> Flow Domain.Types.Station.Station
    getStation stationId = do
      stationId' <- fromMaybeM (InternalError "StationId is missing") stationId
      QStation.findById stationId' >>= fromMaybeM (InternalError "Station not found")

getFrfsSourceAndDestination :: [Either JT.SubwayLegRouteInfo JT.MetroLegRouteInfo] -> (Either FRFSStationAPI (Maybe FRFSStationAPI), Either FRFSStationAPI (Maybe FRFSStationAPI))
getFrfsSourceAndDestination legsRouteInfo =
  let totalLegs = length legsRouteInfo
      source = case safeHead (filter isFirstLeg legsRouteInfo) of
        Nothing -> Right Nothing
        Just (Left subway) -> Left subway.originStop
        Just (Right metro) -> Left metro.originStop
      destination = case safeHead (filter (isLastLeg $ Just totalLegs) legsRouteInfo) of
        Nothing -> Right Nothing
        Just (Left subway) -> Left subway.destinationStop
        Just (Right metro) -> Left metro.destinationStop
   in (source, destination)
  where
    isFirstLeg (Left subway) = subway.subOrder == Just 1
    isFirstLeg (Right metro) = metro.subOrder == Just 1
    isLastLeg totalLegs (Left subway) = subway.subOrder == totalLegs
    isLastLeg totalLegs (Right metro) = metro.subOrder == totalLegs
