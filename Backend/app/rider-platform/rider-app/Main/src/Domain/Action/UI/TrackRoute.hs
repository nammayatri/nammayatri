module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import Data.Function
import qualified Data.List as List
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import Storage.CachedQueries.Merchant.RiderConfig as CQRC
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.Queries.Person as QP
import Tools.Error

getTrackVehicles ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Double ->
    Kernel.Prelude.Maybe Kernel.Prelude.Double ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe DIBC.PlatformType ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Spec.VehicleCategory ->
    Environment.Flow TrackRoute.TrackingResp
  )
getTrackVehicles (mbPersonId, merchantId) routeCode mbCurrentLat mbCurrentLon mbIntegratedBPPConfigId mbMaxBuses mbPlatformType mbSelectedSourceStopId mbSelectedDestinationStopId mbVehicleType = do
  let vehicleType = fromMaybe Spec.BUS mbVehicleType
      maxBuses = fromMaybe 5 mbMaxBuses
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId personCityInfo.merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) (fromMaybe DIBC.APPLICATION mbPlatformType)
  routeIdsToTrack <-
    case (mbSelectedSourceStopId, mbSelectedDestinationStopId) of
      (Just sourceStopId, Just destinationStopId) -> do
        possibleRoutes <- getPossibleRoutesBetweenTwoStops sourceStopId destinationStopId integratedBPPConfig
        let routeCodes = map (.route.code) possibleRoutes
        pure $ if null routeCodes then [routeCode] else routeCodes
      _ -> pure [routeCode]
  logInfo $ "routeIdsToTrack: " <> show routeIdsToTrack
  riderConfig <- CQRC.findByMerchantOperatingCityId personCityInfo.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist personCityInfo.merchantOperatingCityId.getId)
  case (mbCurrentLat, mbCurrentLon) of
    (Just lat, Just lon) -> do
      let currentLocation = LatLong lat lon
      routeStops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig
      let nearestStopRaw = getNearestFromList (comparing (distanceBetweenInMeters currentLocation . (.stopPoint))) routeStops
          nearestStop =
            case nearestStopRaw of
              Just stop
                | Just thresholdInt <- riderConfig.distanceToNearestStopThreshold,
                  distanceBetweenInMeters currentLocation stop.stopPoint
                    <= fromIntegral thresholdInt ->
                  Just stop
              _ -> Nothing

      case nearestStop of
        Nothing -> getTrackWithoutCurrentLocation personId personCityInfo vehicleType maxBuses riderConfig routeIdsToTrack
        Just stop -> do
          vehicleTrackingWithRoutes <- concatMapM (\routeIdToTrack -> map (routeIdToTrack,) <$> trackVehicles personId merchantId personCityInfo.merchantOperatingCityId vehicleType routeIdToTrack (fromMaybe DIBC.APPLICATION mbPlatformType) (Just currentLocation) (Just integratedBPPConfig.id)) routeIdsToTrack
          let deduplicatedVehicles = List.nubBy (\a b -> (snd a).vehicleId == (snd b).vehicleId) vehicleTrackingWithRoutes
          let vehiclesYetToReachSelectedStop = filterVehiclesYetToReachSelectedStop deduplicatedVehicles
          let (confirmedHighBuses, ghostBuses) = List.partition (\a -> ((snd a).vehicleInfo >>= (.routeState)) == Just CQMMB.ConfirmedHigh) vehiclesYetToReachSelectedStop
          let sortedTracking = sortOn (distanceToStop stop . snd) ghostBuses
          pure $
            TrackRoute.TrackingResp
              { vehicleTrackingInfo =
                  map (uncurry mkVehicleTrackingResponse) (confirmedHighBuses <> take maxBuses sortedTracking)
              }
    _ -> getTrackWithoutCurrentLocation personId personCityInfo vehicleType maxBuses riderConfig routeIdsToTrack
  where
    filterVehiclesYetToReachSelectedStop vehicleTracking =
      case mbSelectedSourceStopId of
        Just selectedStopId -> filter (\(_, vt) -> any (\u -> u.stopCode == selectedStopId) vt.upcomingStops) vehicleTracking
        Nothing -> vehicleTracking
    getTrackWithoutCurrentLocation personId personCityInfo vehicleType maxBuses riderConfig routeIdsToTrack = do
      -- Track vehicles for all routes and deduplicate
      vehicleTrackingWithRoutes <- concatMapM (\routeIdToTrack -> map (routeIdToTrack,) <$> trackVehicles personId merchantId personCityInfo.merchantOperatingCityId vehicleType routeIdToTrack (fromMaybe DIBC.APPLICATION mbPlatformType) Nothing mbIntegratedBPPConfigId) routeIdsToTrack
      let deduplicatedVehicles = List.nubBy (\a b -> (snd a).vehicleId == (snd b).vehicleId) vehicleTrackingWithRoutes

      -- Create cache key based on source/destination stops or route codes
      let trackVehicleKey = case (mbSelectedSourceStopId, mbSelectedDestinationStopId) of
            (Just sourceStopId, Just destStopId) -> mkTrackVehicleKeyByStops sourceStopId destStopId vehicleType
            _ -> mkTrackVehicleKey routeCode vehicleType

      cachedResp :: Maybe [Text] <- Redis.safeGet trackVehicleKey
      response <-
        map (uncurry mkVehicleTrackingResponse)
          <$> case cachedResp of
            Just resp -> do
              let vehiclesYetToReachSelectedStop = filterVehiclesYetToReachSelectedStop deduplicatedVehicles
              pure $ filter (\(_, vt) -> vt.vehicleId `elem` resp) vehiclesYetToReachSelectedStop
            Nothing -> do
              let vehiclesYetToReachSelectedStop = filterVehiclesYetToReachSelectedStop deduplicatedVehicles
              let vehiclesToShow = take maxBuses vehiclesYetToReachSelectedStop
              Redis.setExp trackVehicleKey (map ((.vehicleId) . snd) vehiclesToShow) (fromMaybe 900 riderConfig.trackVehicleKeyExpiry)
              pure vehiclesToShow
      pure $
        TrackRoute.TrackingResp
          { vehicleTrackingInfo = response
          }

    mkVehicleTrackingResponse actualRouteCode VehicleTracking {..} =
      TrackRoute.VehicleInfo
        { vehicleId = vehicleId,
          nextStop = nextStop,
          nextStopTravelTime = nextStopTravelTime,
          nextStopTravelDistance = nextStopTravelDistance,
          upcomingStops = upcomingStops,
          delay = delay,
          vehicleInfo =
            maybe
              (TrackRoute.VehicleInfoForRoute Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
              mkVehicleInfo
              vehicleInfo,
          routeCode = actualRouteCode
        }

    mkVehicleInfo VehicleInfo {..} = TrackRoute.VehicleInfoForRoute {..}

    distanceToStop :: RouteStopMapping.RouteStopMapping -> VehicleTracking -> Double
    distanceToStop stop vt =
      case find (\u -> u.stopCode == stop.stopCode) vt.upcomingStops of
        Just u -> maybe (1 / 0) (fromIntegral . getMeters) u.travelDistance
        Nothing -> 1 / 0

    getNearestFromList :: (a -> a -> Ordering) -> [a] -> Maybe a
    getNearestFromList _ [] = Nothing
    getNearestFromList cmp xs = Just (List.minimumBy cmp xs)

mkTrackVehicleKey :: Text -> Spec.VehicleCategory -> Text
mkTrackVehicleKey routeCode vehicleType = "trackVehicles:busNumber:" <> routeCode <> ":" <> show vehicleType

mkTrackVehicleKeyByStops :: Text -> Text -> Spec.VehicleCategory -> Text
mkTrackVehicleKeyByStops sourceStopCode destStopCode vehicleType = "trackVehicles:stops:" <> sourceStopCode <> ":" <> destStopCode <> ":" <> show vehicleType
