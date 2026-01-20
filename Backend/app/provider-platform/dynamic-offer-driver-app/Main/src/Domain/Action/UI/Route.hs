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
  )
where

import qualified Data.List.NonEmpty as NE
import Domain.Action.UI.Ride.EndRide.Internal (pickNWayPoints)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RideRoute as RI
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common (HighPrecMeters)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import SharedLogic.Ride (searchRequestKey)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Maps as Maps

getRoutes :: ServiceFlow m r => (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getRoutes (_, merchantId, merchantOpCityId) enityId req = do
  Maps.getRoutes merchantId merchantOpCityId enityId req

getPickupRoutes :: ServiceFlow m r => (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getPickupRoutes (_, merchantId, merchantOpCityId) enityId req = do
  Maps.getPickupRoutes merchantId merchantOpCityId enityId req

getTripRoutes :: ServiceFlow m r => (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getTripRoutes (personId, merchantId, merchantOpCityId) entityId req = do
  ride' <- QRide.getInProgressByDriverId personId
  case ride' of
    Just ride -> do
      booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      let entityId' = Just $ fromMaybe (getId ride.id) entityId
      let key = searchRequestKey booking.transactionId
      mbRouteInfo :: Maybe RI.RouteInfo <- Redis.runInMultiCloudRedis False $ Redis.withMasterRedis $ Redis.get key
      case mbRouteInfo of
        Just routeInfo -> do
          let src = (NE.head req.waypoints) :: Maps.LatLong
          let points = fromMaybe [] routeInfo.points
          selectedWaypoints <- getSelectedWaypoints src points
          let req' :: Maps.GetRoutesReq =
                Maps.GetRoutesReq
                  { waypoints = NE.fromList ([src] ++ selectedWaypoints ++ [NE.last req.waypoints]),
                    mode = req.mode,
                    calcPoints = req.calcPoints
                  }
          Maps.getTripRoutes merchantId merchantOpCityId entityId' req'
        Nothing -> do
          logDebug $ "No route info found in redis"
          Maps.getTripRoutes merchantId merchantOpCityId entityId' req
    Nothing -> do
      Maps.getTripRoutes merchantId merchantOpCityId entityId req

getSelectedWaypoints :: ServiceFlow m r => Maps.LatLong -> [Maps.LatLong] -> m [Maps.LatLong]
getSelectedWaypoints src waypoints =
  case waypoints of
    (pt : pts) -> do
      let distance = distanceBetweenInMeters src pt
      if distance < (50 :: HighPrecMeters)
        then pure $ pickNWayPoints 5 pts
        else do
          logDebug $ "Distance between src and pt is greater than 50 meters, so not picking any waypoints"
          pure []
    [] -> do
      logDebug $ "No waypoints to pick"
      pure []
