module Lib.JourneyModule.RouteServiceability where

import qualified API.Types.UI.MultimodalConfirm
import qualified BecknV2.FRFS.Enums
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Types.FRFSVehicleServiceTier as DFRFSVehicleServiceTier
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (all, any, catMaybes, concatMap, elem, find, foldr, groupBy, id, length, map, mapM_, null, readMaybe, toList, whenJust)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude hiding (whenJust)
import Kernel.Types.Error
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFSJourneyUtils as JLCF
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.FRFSSeatBooking as SeatBooking
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest

-- | Shared function to build RouteWithLiveVehicle for a single route.
--   Used by both FRFSTicketService and MultimodalConfirm.
buildRouteWithLiveVehicle ::
  CQMMB.RouteWithBuses ->
  NandiTypes.BusScheduleDetails ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Text ->
  Text ->
  [(BecknV2.FRFS.Enums.ServiceTierType, DFRFSVehicleServiceTier.FRFSVehicleServiceTier)] ->
  Maybe LatLong ->
  Int ->
  Flow (Maybe API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle)
buildRouteWithLiveVehicle routeInfo busScheduleDetails integratedBPPConfig fromStopCode toStopCode frfsTierMap mbSourceStopLatLong maxLiveVehicles = do
  route <-
    OTPRest.getRouteByRouteId integratedBPPConfig routeInfo.routeId
      >>= fromMaybeM
        (InvalidRequest $ "Route not found with id: " <> routeInfo.routeId)
  schedulesFork <-
    awaitableFork "getBusScheduleInfo" $
      getBusScheduleInfo busScheduleDetails integratedBPPConfig routeInfo.routeId fromStopCode toStopCode frfsTierMap
  liveVehiclesFork <-
    awaitableFork "getLiveVehicles" $
      getLiveVehicles routeInfo.buses integratedBPPConfig frfsTierMap mbSourceStopLatLong maxLiveVehicles
  schedules <-
    L.await Nothing schedulesFork >>= \case
      Left err -> throwError $ InternalError $ "getBusScheduleInfo fork failed: " <> show err
      Right result -> pure result
  liveVehicles <-
    L.await Nothing liveVehiclesFork >>= \case
      Left err -> throwError $ InternalError $ "getLiveVehicles fork failed: " <> show err
      Right result -> pure result
  logDebug $
    "buildRouteWithLiveVehicle routeId="
      <> routeInfo.routeId
      <> ", scheduleCount="
      <> show (length schedules)
  logDebug $
    "buildRouteWithLiveVehicle routeId="
      <> routeInfo.routeId
      <> ", shortName="
      <> show route.shortName
  if null liveVehicles && null schedules
    then pure Nothing
    else
      pure $
        Just
          API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle
            { liveVehicles,
              schedules,
              routeCode = routeInfo.routeId,
              routeShortName = route.shortName
            }
  where
    getBusScheduleInfo busScheduleDetails' integratedBPPConfig' routeId' fromStopCode' toStopCode' frfsTierMap' = do
      -- Batch-fetch all live info in one Redis HMGET instead of N individual calls
      let vehicleNos = map (.vehicle_no) busScheduleDetails'
      liveInfoResults <- JLCF.getVehicleMetadata vehicleNos integratedBPPConfig'
      let busLiveInfoMap = zip vehicleNos liveInfoResults
      -- Hoist stop index lookup once (same cache key for all vehicles)
      mStopIndices <- JMU.getRouteStopIndices routeId' fromStopCode' toStopCode' integratedBPPConfig'
      catMaybes
        <$> mapM
          ( \detail -> do
              let busLiveInfo = join $ lookup detail.vehicle_no busLiveInfoMap
              logDebug $ "getBusScheduleInfo: getBusLiveInfo vehicle=" <> detail.vehicle_no <> ", found=" <> show (isJust busLiveInfo) <> ", details=" <> show (fmap (\v -> (v.vehicle_number, v.latitude, v.longitude, v.timestamp, v.routes_info, v.bearing)) busLiveInfo)
              mbServiceTier <- JMU.getVehicleServiceTypeFromInMem [integratedBPPConfig'] detail.vehicle_no
              case mbServiceTier of
                Just serviceTier -> do
                  let frfsServiceTier = lookup serviceTier frfsTierMap'
                  -- Get service subtypes from in-memory cache
                  mbServiceSubTypes <- JMU.getVehicleServiceSubTypesFromInMem [integratedBPPConfig'] detail.vehicle_no

                  let combinedTripId = do
                        waybill <- detail.waybill_no
                        tNum <- detail.trip_number
                        return $ waybill <> "-" <> show tNum
                  mbLiveRouteInfo <-
                    JMU.getLiveRouteInfo
                      integratedBPPConfig'
                      detail.vehicle_no
                      routeId'
                  let seatLayoutId = mbLiveRouteInfo >>= (.seatLayoutId)
                      currentTripNum =
                        if serviceTier == BecknV2.FRFS.Enums.PREMIUM
                          then mbLiveRouteInfo >>= (.tripNumber)
                          else Nothing
                  (isAvailable, availableSeatsCount) <- case seatLayoutId of
                    Just layoutId -> case combinedTripId of
                      Nothing -> return (False, Nothing)
                      Just tripId -> do
                        case mStopIndices of
                          Just (fromIdx, toIdx) -> do
                            avail <- SeatBooking.getAvailableSeatCount layoutId tripId fromIdx toIdx
                            logInfo $ "seatAvailability routeId=" <> routeId' <> " tripId=" <> tripId <> " vehicle=" <> detail.vehicle_no <> " available=" <> show avail
                            return (avail > 0, Just avail)
                          _ -> return (True, Nothing)
                    Nothing -> return (True, Nothing)

                  if not isAvailable
                    then return Nothing
                    else do
                      logDebug $ "getBusScheduleInfo: vehicle=" <> detail.vehicle_no <> ", serviceTier=" <> show serviceTier <> ", frfsName=" <> show ((.shortName) <$> frfsServiceTier) <> ", hasLiveInfo=" <> show (isJust busLiveInfo) <> ", eta=" <> show detail.eta <> ", position=" <> show ((\bli -> LatLong bli.latitude bli.longitude) <$> busLiveInfo) <> ", timestamp=" <> show ((.timestamp) <$> busLiveInfo)
                      return . Just $
                        API.Types.UI.MultimodalConfirm.ScheduledVehicleInfo
                          { eta = Just detail.eta,
                            position = (\bli -> LatLong bli.latitude bli.longitude) <$> busLiveInfo,
                            locationUTCTimestamp = posixSecondsToUTCTime . fromIntegral . (.timestamp) <$> busLiveInfo,
                            serviceTierType = serviceTier,
                            serviceTierName = (.shortName) <$> frfsServiceTier,
                            vehicleNumber = detail.vehicle_no,
                            tripId = combinedTripId,
                            serviceSubTypes = mbServiceSubTypes,
                            availableSeats = availableSeatsCount,
                            currentTripNumber = currentTripNum
                          }
                Nothing -> do
                  logError $ "Vehicle info not found for bus: " <> detail.vehicle_no
                  return Nothing
          )
          busScheduleDetails'

    getLiveVehicles busesData integratedBPPConfig' frfsTierMap' mbSourceLatLong maxLiveCount = do
      allVehicles <-
        catMaybes
          <$> mapM
            ( \bus -> do
                mbServiceTier <- JMU.getVehicleServiceTypeFromInMem [integratedBPPConfig'] bus.vehicleNumber
                case mbServiceTier of
                  Just serviceTier -> do
                    let frfsServiceTier = lookup serviceTier frfsTierMap'
                    -- Get service subtypes from in-memory cache
                    mbServiceSubTypes <- JMU.getVehicleServiceSubTypesFromInMem [integratedBPPConfig'] bus.vehicleNumber

                    logDebug $ "getLiveVehicles: vehicle=" <> bus.vehicleNumber <> ", routeId=" <> bus.busData.route_id <> ", serviceTier=" <> show serviceTier <> ", frfsName=" <> show ((.shortName) <$> frfsServiceTier) <> ", position=(" <> show bus.busData.latitude <> "," <> show bus.busData.longitude <> ")" <> ", timestamp=" <> show bus.busData.timestamp <> ", eta=" <> show bus.busData.eta_data <> ", routeState=" <> show bus.busData.route_state <> ", routeNumber=" <> show bus.busData.route_number
                    enrichedEta <-
                      mapM
                        (enrichBusStopETA integratedBPPConfig')
                        (fromMaybe [] bus.busData.eta_data)

                    return . Just $
                      API.Types.UI.MultimodalConfirm.LiveVehicleInfo
                        { eta = Just enrichedEta,
                          number = bus.vehicleNumber,
                          position = LatLong bus.busData.latitude bus.busData.longitude,
                          locationUTCTimestamp = posixSecondsToUTCTime $ fromIntegral bus.busData.timestamp,
                          serviceTierType = serviceTier,
                          serviceTierName = (.shortName) <$> frfsServiceTier,
                          serviceSubTypes = mbServiceSubTypes
                        }
                  Nothing -> do
                    logError $ "Vehicle info not found for bus: " <> bus.vehicleNumber
                    return Nothing
            )
            busesData
      -- Sort by Haversine distance to source stop (closest first) and take top 5
      let sorted = case mbSourceLatLong of
            Just sourceLatLong ->
              sortOn (\v -> distanceBetweenInMeters sourceLatLong v.position) allVehicles
            Nothing -> allVehicles
      pure $ take maxLiveCount sorted

enrichBusStopETA :: DIntegratedBPPConfig.IntegratedBPPConfig -> CQMMB.BusStopETA -> Flow CQMMB.BusStopETA
enrichBusStopETA integratedBPPConfig' eta =
  case eta.stopName of
    Just _ ->
      pure eta
    Nothing -> do
      mbStation <-
        OTPRest.getStationByGtfsIdAndStopCode
          eta.stopCode
          integratedBPPConfig'
      let fetchedName = fmap (.name) mbStation
      pure $ eta {CQMMB.stopName = fetchedName <|> eta.stopName}
