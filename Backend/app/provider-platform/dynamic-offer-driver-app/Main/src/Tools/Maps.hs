{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Tools.Maps
  ( module Reexport,
    -- autoComplete,
    getDistance,
    getEstimatedPickupDistances,
    -- getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
    getPickupRoutes,
    getTripRoutes,
    getDistanceForCancelRide,
    pickService,
    MapsFlow,
  )
where

import Data.Coerce (coerce)
import Data.Singletons.TH
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Merchant.MerchantServiceUsageConfig as DMSUC
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

type MapsFlow m r = (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m)

getDistance ::
  ( MapsFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Maps.SMapsService 'Maps.GetDistances ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig Maps.getDistance

getDistanceForCancelRide ::
  ( MapsFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Maps.SMapsService 'Maps.GetDistancesForCancelRide ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForCancelRide = runWithServiceConfig Maps.getDistance

getEstimatedPickupDistances ::
  ( MapsFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Maps.SMapsService 'Maps.GetEstimatedPickupDistances ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getEstimatedPickupDistances = runWithServiceConfig Maps.getDistances

getRoutes :: MapsFlow m r => Id Merchant -> Maps.SMapsService 'Maps.GetRoutes -> GetRoutesReq -> m GetRoutesResp
getRoutes = runWithServiceConfig Maps.getRoutes

getPickupRoutes :: MapsFlow m r => Id Merchant -> Maps.SMapsService 'Maps.GetPickupRoutes -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes = runWithServiceConfig Maps.getRoutes

getTripRoutes :: MapsFlow m r => Id Merchant -> Maps.SMapsService 'Maps.GetTripRoutes -> GetRoutesReq -> m GetRoutesResp
getTripRoutes = runWithServiceConfig Maps.getRoutes

snapToRoad ::
  ( MapsFlow m r,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters]
  ) =>
  Id Merchant ->
  Maps.SMapsService 'Maps.SnapToRoad ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad

-- not used
-- autoComplete :: MapsFlow m r => Id Merchant -> Maps.SMapsService 'Maps.AutoComplete -> AutoCompleteReq -> m AutoCompleteResp
-- autoComplete = runWithServiceConfig Maps.autoComplete

getPlaceName :: MapsFlow m r => Id Merchant -> Maps.SMapsService 'Maps.GetPlaceName -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.getPlaceName

-- not used
-- getPlaceDetails :: MapsFlow m r => Id Merchant -> Maps.SMapsService 'Maps.GetPlaceDetails -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
-- getPlaceDetails = runWithServiceConfig Maps.getPlaceDetails

runWithServiceConfig ::
  MapsFlow m r =>
  (MapsServiceConfig -> req -> m resp) ->
  Id Merchant ->
  Maps.SMapsService msuc ->
  req ->
  m resp
runWithServiceConfig func merchantId service req = do
  orgMapsServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.MapsService service.getStrictMapsService)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show service))
  case orgMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

pickService ::
  forall (msum :: Maps.MapsServiceUsageMethod) m r.
  (CacheFlow m r, EsqDBFlow m r, SingI msum) =>
  Id Merchant ->
  m (Maps.SMapsService msum)
pickService merchantId = do
  let mapsServiceUsageMethod = fromSing (sing @msum)
  orgMapsConfig <- CQMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  let MapsServiceUsage {..} = getMapsServiceUsage @msum orgMapsConfig
  let percentages =
        [(Maps.Google, googlePercentage), (Maps.OSRM, osrmPercentage), (Maps.MMI, mmiPercentage)] <&> \(element, percentage) -> do
          Random.Percentage {element = Maps.SMapsService element, percentage = fromMaybe 0 percentage}
  if usePercentage
    then do
      result <- Random.getRandomElementUsingPercentages percentages
      logDebug $ "Pick maps service: " <> show mapsServiceUsageMethod <> "; merchantId: " <> merchantId.getId <> "; result: " <> show result
      case result.pickedElement of
        Left err -> do
          logWarning $ "Fail to pick random service: " <> show err <> "; use configured service instead: " <> show mapsService
          pure mapsService
        Right pickedService -> pure $ pickedService
    else pure mapsService

getMapsServiceUsage ::
  forall (msum :: Maps.MapsServiceUsageMethod).
  (SingI msum) =>
  DMSUC.MerchantServiceUsageConfig ->
  Maps.MapsServiceUsage msum
getMapsServiceUsage cfg = do
  let mapsServiceUsageMethod = fromSing (sing @msum)
  case mapsServiceUsageMethod of
    GetDistances -> castMapsServiceUsage @'GetDistances cfg.getDistances
    GetEstimatedPickupDistances -> castMapsServiceUsage @'GetEstimatedPickupDistances cfg.getEstimatedPickupDistances
    GetRoutes -> castMapsServiceUsage @'GetRoutes cfg.getRoutes
    GetPickupRoutes -> castMapsServiceUsage @'GetPickupRoutes cfg.getPickupRoutes
    GetTripRoutes -> castMapsServiceUsage @'GetTripRoutes cfg.getTripRoutes
    SnapToRoad -> castMapsServiceUsage @'SnapToRoad cfg.snapToRoad
    GetPlaceName -> castMapsServiceUsage @'GetPlaceName cfg.getPlaceName
    GetPlaceDetails -> castMapsServiceUsage @'GetPlaceDetails cfg.getPlaceDetails
    AutoComplete -> castMapsServiceUsage @'AutoComplete cfg.autoComplete
    GetDistancesForCancelRide -> castMapsServiceUsage @'GetDistancesForCancelRide cfg.getDistancesForCancelRide

castMapsServiceUsage ::
  forall (msum1 :: Maps.MapsServiceUsageMethod) (msum2 :: Maps.MapsServiceUsageMethod).
  Maps.MapsServiceUsage msum1 ->
  Maps.MapsServiceUsage msum2
castMapsServiceUsage = coerce
