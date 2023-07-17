{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Maps
  ( module Reexport,
    autoComplete,
    getDistance,
    getEstimatedPickupDistances,
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
    getPickupRoutes,
    getTripRoutes,
    getDistanceForCancelRide,
    pickService,
    MapsServiceUsageMethod (..),
  )
where

import Data.Char (toLower)
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import qualified GHC.Show as Show
import Kernel.External.Maps as Reexport hiding
  ( autoComplete,
    getDistance,
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
  )
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Randomizer as Random
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Tools.Error
import Tools.Metrics

getDistance ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  MapsService ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig Maps.getDistance

getDistanceForCancelRide ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  MapsService ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForCancelRide = runWithServiceConfig Maps.getDistance

getDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  MapsService ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithServiceConfig Maps.getDistances

getEstimatedPickupDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  MapsService ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getEstimatedPickupDistances = runWithServiceConfig Maps.getDistances

getRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> MapsService -> GetRoutesReq -> m GetRoutesResp
getRoutes = runWithServiceConfig Maps.getRoutes

getPickupRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> MapsService -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes = runWithServiceConfig Maps.getRoutes

getTripRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> MapsService -> GetRoutesReq -> m GetRoutesResp
getTripRoutes = runWithServiceConfig Maps.getRoutes

snapToRoad ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters]
  ) =>
  Id Merchant ->
  MapsService ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad

autoComplete :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> MapsService -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig Maps.autoComplete

getPlaceName :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> MapsService -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.getPlaceName

getPlaceDetails :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> MapsService -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig Maps.getPlaceDetails

runWithServiceConfig ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  (MapsServiceConfig -> req -> m resp) ->
  Id Merchant ->
  MapsService ->
  req ->
  m resp
runWithServiceConfig func merchantId service req = do
  orgMapsServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.MapsService service)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show service))
  case orgMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

pickService :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> MapsServiceUsageMethod -> m MapsService
pickService merchantId mapsServiceUsageMethod = do
  orgMapsConfig <- CQMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  let MapsServiceUsage {..} = getMapsServiceUsage orgMapsConfig mapsServiceUsageMethod
  let percentages =
        [(Google, googlePercentage), (OSRM, osrmPercentage), (MMI, mmiPercentage)] <&> \(element, percentage) -> do
          Random.Percentage {element, percentage = fromMaybe 0 percentage}
  if usePercentage
    then do
      result <- Random.getRandomElementUsingPercentages percentages
      logDebug $ "Pick maps service: " <> show mapsServiceUsageMethod <> "; merchantId: " <> merchantId.getId <> "; result: " <> show result
      case result.pickedElement of
        Left err -> do
          logWarning $ "Fail to pick random service: " <> show err <> "; use configured service instead: " <> show mapsService
          pure mapsService
        Right pickedService -> pure pickedService
    else pure mapsService

data MapsServiceUsageMethod
  = GetDistances
  | GetEstimatedPickupDistances
  | GetRoutes
  | GetPickupRoutes
  | GetTripRoutes
  | SnapToRoad
  | GetPlaceName
  | GetPlaceDetails
  | AutoComplete
  | GetDistancesForCancelRide

-- TODO test
instance Show MapsServiceUsageMethod where
  show x = case shows x "" of
    [] -> []
    y : ys -> toLower y : ys

getMapsServiceUsage :: MerchantServiceUsageConfig -> MapsServiceUsageMethod -> MapsServiceUsage
getMapsServiceUsage cfg = \case
  GetDistances -> cfg.getDistances
  GetEstimatedPickupDistances -> cfg.getEstimatedPickupDistances
  GetRoutes -> cfg.getRoutes
  GetPickupRoutes -> cfg.getPickupRoutes
  GetTripRoutes -> cfg.getTripRoutes
  SnapToRoad -> cfg.snapToRoad
  GetPlaceName -> cfg.getPlaceName
  GetPlaceDetails -> cfg.getPlaceDetails
  AutoComplete -> cfg.autoComplete
  GetDistancesForCancelRide -> cfg.getDistancesForCancelRide
