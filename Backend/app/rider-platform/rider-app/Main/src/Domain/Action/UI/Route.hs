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
import Control.Lens ((^?), _head)
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
  mbRide <- mapM (\rid -> QRide.findById rid >>= fromMaybeM (RideNotFound rid.getId)) rideId
  service <- getService merchantConfig mbRide
  let req = Maps.GetRoutesReq {..}
  resp <- Maps.getPickupRoutes merchantId mocId service entityId req
  whenJust mbRide $ \ride ->
    when (fromMaybe 0 ride.pickupRouteCallCount == 0) $ do
      let mbSpeed = do
            routeInfo <- resp ^? _head
            dist <- routeInfo.distance
            dur <- routeInfo.duration
            guard (dur > 0)
            pure $ fromIntegral dist / fromIntegral dur
      QRide.updatePickupSpeedInMPS mbSpeed ride.id
  return resp
  where
    getService merchantConfig mbRide = do
      case (mbRide, merchantConfig.getFirstPickupRoute) of
        (Just ride, Just firstPickupService) -> do
          let pikcupRouteCalls = fromMaybe 0 (ride.pickupRouteCallCount)
          if pikcupRouteCalls == 0
            then do
              QRide.updatePickupRouteCallCount (Just $ pikcupRouteCalls + 1) ride.id
              return firstPickupService
            else return merchantConfig.getPickupRoutes
        _ -> return merchantConfig.getPickupRoutes

getTripRoutes :: ServiceFlow m r => (Id DP.Person, Id Merchant.Merchant) -> Maybe Text -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getTripRoutes (personId, merchantId) entityId req = do
  Maps.getTripRoutes personId merchantId Nothing entityId req
