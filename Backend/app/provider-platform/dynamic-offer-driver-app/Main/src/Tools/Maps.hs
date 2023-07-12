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
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DOSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
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
import Kernel.Randomizer
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QOMC
import qualified Text.Show
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
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig Maps.getDistance "getDistances" (.getDistances)

getDistanceForCancelRide ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForCancelRide = runWithServiceConfig Maps.getDistance "getDistancesForCancelRide" (.getDistancesForCancelRide)

getDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithServiceConfig Maps.getDistances "getDistances" (.getDistances)

getEstimatedPickupDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getEstimatedPickupDistances = runWithServiceConfig Maps.getDistances "getEstimatedPickupDistances" (.getEstimatedPickupDistances)

getRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getRoutes = runWithServiceConfig Maps.getRoutes "getRoutes" (.getRoutes)

getPickupRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes = runWithServiceConfig Maps.getRoutes "getPickupRoutes" (.getPickupRoutes)

getTripRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getTripRoutes = runWithServiceConfig Maps.getRoutes "getTripRoutes" (.getTripRoutes)

snapToRoad ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters]
  ) =>
  Id Merchant ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad "snapToRoad" (.snapToRoad)

autoComplete :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig Maps.autoComplete "autoComplete" (.autoComplete)

getPlaceName :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.getPlaceName "getPlaceName" (.getPlaceName)

getPlaceDetails :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig Maps.getPlaceDetails "getPlaceDetails" (.getPlaceDetails)

runWithServiceConfig ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  (MapsServiceConfig -> req -> m resp) ->
  Text ->
  (MerchantServiceUsageConfig -> MapsServiceUsage) ->
  Id Merchant ->
  req ->
  m resp
runWithServiceConfig func fieldName getMapsServiceUsage orgId req = do
  orgMapsConfig <- QOMC.findByMerchantId orgId >>= fromMaybeM (MerchantServiceUsageConfigNotFound orgId.getId)
  let mapsServiceUsage = getMapsServiceUsage orgMapsConfig
  service <- pickService orgId fieldName mapsServiceUsage
  orgMapsServiceConfig <-
    QOMSC.findByMerchantIdAndService orgId (DOSC.MapsService service)
      >>= fromMaybeM (MerchantServiceConfigNotFound orgId.getId "Maps" (show service))
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

-- TODO move to Lib
pickService :: MonadFlow m => Id Merchant -> Text -> MapsServiceUsage -> m MapsService
pickService merchantId fieldName MapsServiceUsage {..} = do
  let percentages = [(Google, googlePercentage), (OSRM, osrmPercentage), (MMI, mmiPercentage)]
  let catPercentages = mapMaybe snd percentages
  unless (null catPercentages || (sum catPercentages == 100)) $
    throwError (InternalError $ "Sum percentage for merchantId " <> merchantId.getId <> " and field name " <> fieldName <> " should be 100.")
  if usePercentage
    then do
      when (null catPercentages) $
        -- add logs and not throw error??
        throwError (InternalError $ "Percentages not configured for merchantId " <> merchantId.getId <> " and field name " <> fieldName <> " should be 100.")
      random <- getRandomInRange (1, 100)
      let (ranges, mbPickedService) = foldl (foldRange random) ([], Nothing) percentages
      logDebug $ "Pick maps service: " <> fieldName <> "; merchantId: " <> merchantId.getId <> "; random: " <> show random <> "; ranges: " <> show ranges <> "; picked: " <> show mbPickedService
      case mbPickedService of
        Nothing -> do
          -- impossible
          logWarning $ "Fail to pick random service, use configured service instead: " <> show mapsService
          pure mapsService
        Just pickedService -> pure pickedService
    else pure mapsService

data ServiceRange = ServiceRange
  { service :: MapsService,
    percentage :: Maybe Int,
    range :: Range
  }
  deriving (Show)

newtype Range = Range (Int, Int)

instance Show Range where
  show (Range (x, y)) = show x <> " < random <= " <> show y

-- TODO find proper function in lib for randoms, or move code to lib
foldRange :: Int -> ([ServiceRange], Maybe MapsService) -> (MapsService, Maybe Int) -> ([ServiceRange], Maybe MapsService)
foldRange random (serviceRanges, mbPickedService) (service, percentage) = do
  let Range (_rangeFloor, rangeCeil) = maybe (Range (0, 0)) (.range) (listToMaybe serviceRanges)
      rangeCeil' = rangeCeil + fromMaybe 0 percentage
      range' = Range (rangeCeil, rangeCeil')
  let serviceRange' =
        ServiceRange
          { service,
            percentage,
            range = range'
          }
  let mbPickedServer' = case mbPickedService of
        Nothing -> if random <= rangeCeil' then Just service else Nothing
        Just pickedService -> Just pickedService
  (serviceRange' : serviceRanges, mbPickedServer')
