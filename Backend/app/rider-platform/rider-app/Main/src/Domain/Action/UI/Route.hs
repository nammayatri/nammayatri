{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Route
  ( Maps.GetRoutesReq,
    Maps.GetRoutesResp,
    getRoutes,
    getPickupRoutes,
    getTripRoutes,
    GetPickupRoutesReq (..),
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow, fromMaybeM)
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as Maps

data GetPickupRoutesReq = GetPickupRoutesReq
  { waypoints :: NonEmpty Maps.LatLong,
    mode :: Maybe Maps.TravelMode,
    calcPoints :: Bool,
    rideId :: Maybe (Id Ride)
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

getRoutes :: (ServiceFlow m r, EsqDBReplicaFlow m r) => (Id DP.Person, Id Merchant.Merchant) -> Maybe Text -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getRoutes (personId, merchantId) entityId req = do
  Maps.getRoutes Nothing personId merchantId Nothing entityId req

getPickupRoutes ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  (Id DP.Person, Id Merchant.Merchant) ->
  Maybe Text ->
  GetPickupRoutesReq ->
  m Maps.GetRoutesResp
getPickupRoutes (personId, merchantId) entityId GetPickupRoutesReq {..} = do
  mocId <- Maps.getMerchantOperatingCityId personId Nothing
  merchantConfig <- QMSUC.findByMerchantOperatingCityId mocId >>= fromMaybeM (MerchantServiceUsageConfigNotFound mocId.getId)
  service <- getService merchantConfig
  let req = Maps.GetRoutesReq {..}
  Maps.getPickupRoutes merchantId mocId service entityId req
  where
    getService merchantConfig = do
      case (rideId, merchantConfig.getFirstPickupRoute) of
        (Just rid, Just firstPickupService) -> do
          ride <- QRide.findById rid >>= fromMaybeM (RideNotFound rid.getId)
          let pikcupRouteCalls = fromMaybe 0 (ride.pickupRouteCallCount)
          QRide.updatePickupRouteCallCount (Just $ pikcupRouteCalls + 1) rid
          if pikcupRouteCalls == 0
            then return firstPickupService
            else return merchantConfig.getPickupRoutes
        _ -> return merchantConfig.getPickupRoutes

getTripRoutes :: ServiceFlow m r => (Id DP.Person, Id Merchant.Merchant) -> Maybe Text -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getTripRoutes (personId, merchantId) entityId req = do
  Maps.getTripRoutes personId merchantId Nothing entityId req
