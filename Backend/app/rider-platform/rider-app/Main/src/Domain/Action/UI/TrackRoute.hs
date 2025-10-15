module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import Data.Function
import qualified Data.List as List
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station as Station
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
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
getTrackVehicles (mbPersonId, merchantId) routeCode _mbCurrentLat _mbCurrentLon mbIntegratedBPPConfigId mbMaxBuses mbPlatformType mbSelectedSourceStopId mbSelectedDestinationStopId mbVehicleType = do
  let vehicleType = fromMaybe Spec.BUS mbVehicleType
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
  let maxBuses = fromMaybe 5 $ mbMaxBuses <|> riderConfig.maxNearbyBuses
  mbSourceStop <- maybe (pure Nothing) (\stopCode -> OTPRest.getStationByGtfsIdAndStopCode stopCode integratedBPPConfig) mbSelectedSourceStopId
  let currentLocation = (\ss -> LatLong <$> ss.lat <*> ss.lon) =<< mbSourceStop
  vehicleTrackingWithRoutes <- concatMapM (\routeIdToTrack -> map (routeIdToTrack,) <$> trackVehicles personId merchantId personCityInfo.merchantOperatingCityId vehicleType routeIdToTrack (fromMaybe DIBC.APPLICATION mbPlatformType) currentLocation (Just integratedBPPConfig.id)) routeIdsToTrack
  let deduplicatedVehicles = List.nubBy (\a b -> (snd a).vehicleId == (snd b).vehicleId) vehicleTrackingWithRoutes
  let vehiclesYetToReachSelectedStop = filterVehiclesYetToReachSelectedStop deduplicatedVehicles
  let (confirmedHighBuses, ghostBuses) = List.partition (\a -> ((snd a).vehicleInfo >>= (.routeState)) == Just CQMMB.ConfirmedHigh) vehiclesYetToReachSelectedStop
  let sortedTracking = sortOn (distanceToStop mbSourceStop . snd) ghostBuses
  pure $
    TrackRoute.TrackingResp
      { vehicleTrackingInfo =
          map (uncurry mkVehicleTrackingResponse) (confirmedHighBuses <> take maxBuses sortedTracking)
      }
  where
    filterVehiclesYetToReachSelectedStop vehicleTracking =
      case mbSelectedSourceStopId of
        Just selectedStopId -> filter (\(_, vt) -> any (\u -> u.stopCode == selectedStopId) vt.upcomingStops) vehicleTracking
        Nothing -> vehicleTracking

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

    distanceToStop :: Maybe Station.Station -> VehicleTracking -> Double
    distanceToStop mbSourceStop vt =
      case find (\u -> Just u.stopCode == fmap (.code) mbSourceStop) vt.upcomingStops of
        Just u -> maybe (1 / 0) (fromIntegral . getMeters) u.travelDistance
        Nothing -> 1 / 0
