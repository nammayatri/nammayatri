module Lib.JourneyLeg.Common.FRFS where

import API.Types.RiderPlatform.Management.Endpoints.FRFSTicket (FRFSStationAPI)
import qualified API.Types.UI.FRFSTicketService as API
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Applicative
import Data.List (sortBy)
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Action.UI.Location (makeLocationAPIEntity)
import Domain.Types.Booking.API as DBA
import Domain.Types.FRFSQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import Domain.Types.Station
import Domain.Types.StationType
import Domain.Types.Trip as DTrip
import Domain.Utils (safeHead)
import EulerHS.Prelude (comparing, (+||), (||+))
import ExternalBPP.CallAPI as CallExternalBPP
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface.Types as EMTypes
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Esqueleto as DB
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
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.Station as QStation
import Tools.Error

getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig]) => DTrip.MultimodalTravelMode -> Id FRFSSearch -> [APITypes.RiderLocationReq] -> Bool -> m JT.JourneyLegState
getState mode searchId riderLastPoints isLastCompleted = do
  mbBooking <- QTBooking.findBySearchId searchId
  let userPosition = (.latLong) <$> listToMaybe riderLastPoints
  case mbBooking of
    Just booking -> do
      (statusChanged, newStatus) <- processOldStatus booking.journeyLegStatus booking.toStationId
      when statusChanged $ QTBooking.updateJourneyLegStatus (Just newStatus) booking.id
      journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
      vehicleTrackingAndPosition <- do
        let mbRouteStations :: Maybe [API.FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
        getVehiclePosition booking.riderId booking.merchantId booking.merchantOperatingCityId newStatus userPosition booking.vehicleType mbRouteStations
      let vehiclePosition = snd <$> vehicleTrackingAndPosition
          nextStopDetails = fst <$> vehicleTrackingAndPosition
      return $
        JT.JourneyLegState
          { status = if newStatus == JPT.InPlan then JT.getFRFSLegStatusFromBooking booking else newStatus,
            userPosition,
            vehiclePosition = vehiclePosition,
            nextStop = nextStopDetails <&> (.nextStop),
            nextStopTravelTime = nextStopDetails >>= (.nextStopTravelTime),
            nextStopTravelDistance = nextStopDetails <&> (.nextStopTravelDistance),
            legOrder = journeyLegOrder,
            statusChanged,
            mode
          }
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      (statusChanged, newStatus) <- processOldStatus searchReq.journeyLegStatus searchReq.toStationId
      when statusChanged $ QFRFSSearch.updateJourneyLegStatus (Just newStatus) searchReq.id
      journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest "JourneySearchData not found")
      vehicleTrackingAndPosition <- do
        case journeyLegInfo.pricingId of
          Just quoteId -> do
            mbQuote <- QFRFSQuote.findById (Id quoteId)
            case mbQuote of
              Just quote -> do
                let mbRouteStations :: Maybe [API.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
                getVehiclePosition quote.riderId quote.merchantId searchReq.merchantOperatingCityId newStatus userPosition searchReq.vehicleType mbRouteStations
              Nothing -> pure Nothing
          Nothing -> pure Nothing
      let vehiclePosition = snd <$> vehicleTrackingAndPosition
          nextStopDetails = fst <$> vehicleTrackingAndPosition
      return $
        JT.JourneyLegState
          { status = newStatus,
            userPosition,
            vehiclePosition = vehiclePosition,
            nextStop = nextStopDetails <&> (.nextStop),
            nextStopTravelTime = nextStopDetails >>= (.nextStopTravelTime),
            nextStopTravelDistance = nextStopDetails <&> (.nextStopTravelDistance),
            legOrder = journeyLegInfo.journeyLegOrder,
            statusChanged,
            mode
          }
  where
    getVehiclePosition :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig]) => Id DPerson.Person -> Id DMerchant.Merchant -> Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> JPT.JourneyLegStatus -> Maybe LatLong -> Spec.VehicleCategory -> Maybe [API.FRFSRouteStationsAPI] -> m (Maybe (VehicleTracking, LatLong))
    getVehiclePosition riderId merchantId merchantOperatingCityId journeyStatus riderPosition vehicleType = \case
      Just routesStations ->
        case listToMaybe routesStations of
          Just routeStations -> do
            vehicleTracking <- trackVehicles riderId merchantId merchantOperatingCityId vehicleType routeStations.code
            if isUpcomingJourneyLeg journeyStatus
              then do
                let vehicleTrackingWithLatLong :: [(VehicleTracking, Double, Double)] = mapMaybe (\vehicleTrack -> (vehicleTrack,,) <$> vehicleTrack.vehicleInfo.latitude <*> vehicleTrack.vehicleInfo.longitude) vehicleTracking
                    mbStartStation = find (\station -> station.stationType == Just START) routeStations.stations
                    upcomingNearestVehicles =
                      sortBy
                        (comparing (\(vehicleTrack, _, _) -> vehicleTrack.nextStop.sequenceNum) <> comparing (\(vehicleTrack, _, _) -> vehicleTrack.nextStopTravelDistance))
                        $ filter
                          (\(vehicleTrack, _, _) -> maybe False (\startStation -> maybe False (\stationSequenceNum -> vehicleTrack.nextStop.sequenceNum <= stationSequenceNum) startStation.sequenceNum) mbStartStation)
                          vehicleTrackingWithLatLong
                pure ((\(vehicleTrack, lat, lon) -> (vehicleTrack, LatLong {..})) <$> listToMaybe upcomingNearestVehicles)
              else
                if isOngoingJourneyLeg journeyStatus
                  then do
                    case riderPosition of
                      Just riderLocation -> do
                        let vehicleTrackWithLatLong :: [(VehicleTracking, Double, Double)] = mapMaybe (\vehicleTrack -> (vehicleTrack,,) <$> vehicleTrack.vehicleInfo.latitude <*> vehicleTrack.vehicleInfo.longitude) vehicleTracking
                            nearestVehicleToUser = sortBy (comparing (\(_, lat, lon) -> distanceBetweenInMeters LatLong {..} riderLocation)) vehicleTrackWithLatLong
                        pure ((\(vehicleTrack, lat, lon) -> (vehicleTrack, LatLong {..})) <$> listToMaybe nearestVehicleToUser)
                      Nothing -> pure Nothing
                  else pure Nothing
          Nothing -> pure Nothing
      Nothing -> pure Nothing

    isOngoingJourneyLeg :: JPT.JourneyLegStatus -> Bool
    isOngoingJourneyLeg legStatus = legStatus `elem` [JPT.Ongoing]

    isUpcomingJourneyLeg :: JPT.JourneyLegStatus -> Bool
    isUpcomingJourneyLeg legStatus = legStatus `elem` [JPT.InPlan]

    processOldStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe JPT.JourneyLegStatus -> Id Station -> m (Bool, JPT.JourneyLegStatus)
    processOldStatus mbOldStatus toStationId = do
      mbToStation <- QStation.findById toStationId
      let mbToLatLong = LatLong <$> (mbToStation >>= (.lat)) <*> (mbToStation >>= (.lon))
      let oldStatus = fromMaybe (if isLastCompleted then JPT.Ongoing else JPT.InPlan) mbOldStatus
      return $ maybe (False, oldStatus) (\latLong -> updateJourneyLegStatus mode riderLastPoints latLong oldStatus isLastCompleted) mbToLatLong

getFare :: (CoreMetrics m, CacheFlow m r, EncFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => DMerchant.Merchant -> MerchantOperatingCity -> Spec.VehicleCategory -> [FRFSRouteDetails] -> m (Maybe JT.GetFareResponse)
getFare merchant merchantOperatingCity vehicleCategory routeDetails = do
  QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory)
    >>= \case
      Just bapConfig -> do
        try @_ @SomeException
          ( mapM
              ( \FRFSRouteDetails {..} ->
                  case routeCode of
                    Just routeCode' -> CallExternalBPP.getFares Nothing merchant merchantOperatingCity bapConfig routeCode' startStationCode endStationCode vehicleCategory DIBC.MULTIMODAL
                    Nothing -> return []
              )
              routeDetails
          )
          >>= \case
            Right [] -> do
              logError $ "Getting Empty Fares for Vehicle Category : " <> show vehicleCategory
              return Nothing
            Right farePerRouteAcrossVehicleServiceTiers -> do
              let farePerRoute = catMaybes (listToMaybe <$> farePerRouteAcrossVehicleServiceTiers)
              if length farePerRoute /= length farePerRouteAcrossVehicleServiceTiers
                then do
                  logError $ "Not Getting Fares for All Transit Routes for Vehicle Category : " <> show vehicleCategory
                  return Nothing
                else do
                  let totalFare = sum $ map ((.getHighPrecMoney) . (.amount) . (.price)) farePerRoute
                  return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = totalFare}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = totalFare}})
            Left err -> do
              logError $ "Exception Occured in Get Fare for Vehicle Category : " <> show vehicleCategory <> ", Error : " <> show err
              return Nothing
      Nothing -> do
        logError $ "Did not get Beckn Config for Vehicle Category : " <> show vehicleCategory
        return Nothing

getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id FRFSSearch -> Maybe HighPrecMoney -> Maybe Distance -> Maybe Seconds -> m JT.LegInfo
getInfo searchId fallbackFare distance duration = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      JT.mkLegInfoFromFrfsBooking booking distance duration
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      JT.mkLegInfoFromFrfsSearchRequest searchReq fallbackFare distance duration

search :: JT.SearchRequestFlow m r c => Spec.VehicleCategory -> Id DPerson.Person -> Id DMerchant.Merchant -> Int -> Context.City -> DJourneyLeg.JourneyLeg -> m JT.SearchResponse
search vehicleCategory personId merchantId quantity city journeyLeg = do
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
      fromStationCode <- ((journeyLeg.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "From station gtfsId not found")
      toStationCode <- ((journeyLeg.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "To station gtfsId not found")
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
              createdAt = now,
              updatedAt = now
            }

    getJourneyRouteDetails :: JT.SearchRequestFlow m r c => [EMTypes.MultiModalRouteDetails] -> MerchantOperatingCity -> DIBC.IntegratedBPPConfig -> m [JPT.MultiModalJourneyRouteDetails]
    getJourneyRouteDetails routeDetails merchantOpCity integratedBPPConfig = do
      mapM transformJourneyRouteDetails routeDetails
      where
        transformJourneyRouteDetails :: JT.SearchRequestFlow m r c => EMTypes.MultiModalRouteDetails -> m JPT.MultiModalJourneyRouteDetails
        transformJourneyRouteDetails rd = do
          fromStationCode <- ((rd.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "From station gtfsId not found")
          toStationCode <- ((rd.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "To station gtfsId not found")
          routeCode <- (rd.gtfsId <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "Route gtfsId not found")
          fromStation <- createStationIfRequired (rd.fromStopDetails >>= (.name)) fromStationCode rd.startLocation.latLng.latitude rd.startLocation.latLng.longitude merchantOpCity integratedBPPConfig
          toStation <- createStationIfRequired (rd.toStopDetails >>= (.name)) toStationCode rd.endLocation.latLng.latitude rd.endLocation.latLng.longitude merchantOpCity integratedBPPConfig
          route <- QRoute.findByRouteCode routeCode integratedBPPConfig.id
          return
            JPT.MultiModalJourneyRouteDetails
              { platformNumber = rd.fromStopDetails >>= (.platformCode),
                lineColorCode = EMTypes.color rd,
                lineColor = EMTypes.shortName rd,
                frequency = EMTypes.frequency rd,
                subLegOrder = Just (EMTypes.subLegOrder rd),
                routeLongName = EMTypes.longName rd,
                fromStationId = fmap (.id) fromStation,
                toStationId = fmap (.id) toStation,
                routeId = fmap (.id) route
              }

    getFrfsRouteDetails :: JT.SearchRequestFlow m r c => [EMTypes.MultiModalRouteDetails] -> m [FRFSRouteDetails]
    getFrfsRouteDetails routeDetails = do
      mapM
        ( \rd -> do
            startStationCode <- ((rd.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "From station gtfsId not found")
            endStationCode <- ((rd.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "To station gtfsId not found")
            routeCode <- (rd.gtfsId <&> gtfsIdtoDomainCode) & fromMaybeM (InvalidRequest "Route gtfsId not found")
            return $ FRFSRouteDetails {routeCode = Just routeCode, ..}
        )
        routeDetails

confirm :: JT.ConfirmFlow m r c => Id DPerson.Person -> Id DMerchant.Merchant -> Id FRFSSearch -> Maybe (Id FRFSQuote) -> Bool -> Bool -> m ()
confirm personId merchantId searchId mbQuoteId skipBooking bookingAllowed = do
  mbBooking <- QTBooking.findBySearchId searchId -- if booking already there no need to confirm again
  when (not skipBooking && bookingAllowed && isNothing mbBooking) $ do
    quoteId <- mbQuoteId & fromMaybeM (InvalidRequest "You can't confirm bus before getting the fare")
    void $ FRFSTicketService.postFrfsQuoteConfirm (Just personId, merchantId) quoteId

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

getLegSourceAndDestination :: Maybe JT.LegInfo -> Bool -> DBA.JourneyLocation
getLegSourceAndDestination mbLegInfo getSource =
  case mbLegInfo of
    Nothing -> Null
    Just legInfo -> case legInfo.legExtraInfo of
      JT.Walk legExtraInfo ->
        if getSource
          then DBA.Taxi $ makeLocationAPIEntity legExtraInfo.origin
          else DBA.Taxi $ makeLocationAPIEntity legExtraInfo.destination
      JT.Taxi legExtraInfo ->
        if getSource
          then DBA.Taxi $ makeLocationAPIEntity legExtraInfo.origin
          else DBA.Taxi $ makeLocationAPIEntity legExtraInfo.destination
      JT.Subway legExtraInfo ->
        let (source, destination) = getFrfsSourceAndDestination (map Left legExtraInfo.routeInfo)
         in if getSource
              then case source of
                Right _ -> Null
                Left src -> DBA.Frfs src
              else case destination of
                Right _ -> Null
                Left dst -> DBA.Frfs dst
      JT.Metro legExtraInfo ->
        let (source, destination) = getFrfsSourceAndDestination (map Right legExtraInfo.routeInfo)
         in if getSource
              then case source of
                Right _ -> Null
                Left src -> DBA.Frfs src
              else case destination of
                Right _ -> Null
                Left dst -> DBA.Frfs dst
      JT.Bus legExtraInfo ->
        if getSource
          then DBA.Frfs legExtraInfo.originStop
          else DBA.Frfs legExtraInfo.destinationStop

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
