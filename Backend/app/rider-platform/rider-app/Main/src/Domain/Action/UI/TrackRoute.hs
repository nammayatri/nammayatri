module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import Data.Function
import qualified Data.List as List
import qualified Data.Map as M
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
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
getTrackVehicles (mbPersonId, merchantId) routeCode _mbCurrentLat _mbCurrentLon mbIntegratedBPPConfigId mbMaxBuses mbPlatformType mbSelectedDestinationStopId mbSelectedSourceStopId mbVehicleType = do
  let vehicleType = fromMaybe Spec.BUS mbVehicleType
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId personCityInfo.merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) (fromMaybe DIBC.APPLICATION mbPlatformType)
  logInfo $ "mbSelectedSourceStopId: " <> show mbSelectedSourceStopId <> " and mbSelectedDestinationStopId: " <> show mbSelectedDestinationStopId
  routeIdsToTrack <-
    case (mbSelectedSourceStopId, mbSelectedDestinationStopId) of
      (Just sourceStopId, Just destinationStopId) -> do
        if isRouteBasedVehicleTracking integratedBPPConfig
          then pure [routeCode]
          else do
            routeCodes <- JMU.getRouteCodesFromTo sourceStopId destinationStopId integratedBPPConfig
            logInfo $ "possibleRoutes between two stops: " <> show routeCodes
            pure $ List.nub $ [routeCode] <> routeCodes
      _ -> pure [routeCode]
  riderConfig <- CQRC.findByMerchantOperatingCityId personCityInfo.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist personCityInfo.merchantOperatingCityId.getId)
  let maxBuses = fromMaybe 5 $ mbMaxBuses <|> riderConfig.maxNearbyBuses
  mbSourceStop <- maybe (pure Nothing) (\stopCode -> OTPRest.getStationByGtfsIdAndStopCode stopCode integratedBPPConfig) mbSelectedSourceStopId
  let currentLocation = (\ss -> LatLong <$> ss.lat <*> ss.lon) =<< mbSourceStop
  vehicleTrackingWithRoutes <- concatMapM (\routeIdToTrack -> map (routeIdToTrack,) <$> trackVehicles personId merchantId personCityInfo.merchantOperatingCityId vehicleType routeIdToTrack (fromMaybe DIBC.APPLICATION mbPlatformType) currentLocation (Just integratedBPPConfig.id)) routeIdsToTrack
  let deduplicatedVehicles = List.nubBy (\a b -> (snd a).vehicleId == (snd b).vehicleId) vehicleTrackingWithRoutes
  logInfo $ "deduplicatedVehicles: " <> show deduplicatedVehicles
  let vehiclesYetToReachSelectedStop = filterVehiclesYetToReachSelectedStop deduplicatedVehicles
  let (confirmedHighBuses, ghostBuses) = List.partition (\a -> ((snd a).vehicleInfo >>= (.routeState)) == Just CQMMB.ConfirmedHigh) vehiclesYetToReachSelectedStop
  let sortedTracking = sortOn (distanceToStop currentLocation . snd) ghostBuses
  let sortedConfirmed = sortOn (distanceToStop currentLocation . snd) confirmedHighBuses
  let (nearestXBuses, restOfBuses) = splitAt maxBuses $ sortedConfirmed <> sortedTracking
  serviceTiersOfSelectedBuses :: [(Maybe Spec.ServiceTierType, (Text, VehicleTracking))] <- mapM (\vehicle -> (,vehicle) <$> JMU.getVehicleServiceTypeFromInMem [integratedBPPConfig] (snd vehicle).vehicleId) nearestXBuses
  serviceTiersOfRemainingBuses :: [(Maybe Spec.ServiceTierType, (Text, VehicleTracking))] <- mapM (\vehicle -> (,vehicle) <$> JMU.getVehicleServiceTypeFromInMem [integratedBPPConfig] (snd vehicle).vehicleId) restOfBuses
  let alreadySelectedServiceTiers :: [Maybe Spec.ServiceTierType] = List.nub $ map fst serviceTiersOfSelectedBuses
  let oneFromEachRemaining :: [(Maybe Spec.ServiceTierType, (Text, VehicleTracking))] = filter (\(st, _) -> not $ st `elem` alreadySelectedServiceTiers) . M.toList $ M.fromList serviceTiersOfRemainingBuses
  let allBuses :: [(Maybe Spec.ServiceTierType, (Text, VehicleTracking))] = serviceTiersOfSelectedBuses <> oneFromEachRemaining

  pure $
    TrackRoute.TrackingResp
      { vehicleTrackingInfo =
          map (uncurry mkVehicleTrackingResponse) allBuses
      }
  where
    filterVehiclesYetToReachSelectedStop vehicleTracking =
      case mbSelectedSourceStopId of
        Just selectedStopId -> filter (\(_, vt) -> any (\u -> u.stopCode == selectedStopId) vt.upcomingStops) vehicleTracking
        Nothing -> vehicleTracking

    mkVehicleTrackingResponse serviceTierType (actualRouteCode, VehicleTracking {..}) =
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
          routeCode = actualRouteCode,
          routeShortName = routeShortName,
          serviceTierType = serviceTierType
        }

    mkVehicleInfo VehicleInfo {..} = TrackRoute.VehicleInfoForRoute {..}

    distanceToStop :: Maybe LatLong -> VehicleTracking -> Maybe HighPrecMeters
    distanceToStop mbSourceStopLocation vt = do
      let mbVehicleLocation = (vt.vehicleInfo >>= \a -> LatLong <$> (a.latitude) <*> (a.longitude))
      distanceBetweenInMeters <$> mbVehicleLocation <*> mbSourceStopLocation

    isRouteBasedVehicleTracking :: DIBC.IntegratedBPPConfig -> Bool
    isRouteBasedVehicleTracking config = case config.providerConfig of
      DIBC.ONDC DIBC.ONDCBecknConfig {routeBasedVehicleTracking} -> fromMaybe False routeBasedVehicleTracking
      _ -> False
