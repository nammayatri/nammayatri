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
    NavigationRouteReq (..),
    getNavigationRoute,
  )
where

import Control.Applicative ((<|>))
import qualified Data.List.NonEmpty as NE
import Domain.Action.UI.Ride.EndRide.Internal (pickNWayPoints)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Fields
import qualified Domain.Types.RideRoute as RI
import Kernel.External.Maps.OSRM.Config (OSRMCfg (..))
import qualified Kernel.External.Maps.OSRM.RoadsClient as OSRM
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common (HighPrecMeters)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import SharedLogic.Ride (searchRequestKey)
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
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
      booking <- QBookingLite.findByIdLite ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      let entityId' = Just $ fromMaybe (getId ride.id) entityId
      let key = searchRequestKey booking.transactionId
      let rideHasStops = fromMaybe False ride.hasStops
      mbRouteInfo :: Maybe RI.RouteInfo <- Redis.runInMultiCloudRedisMaybeResult $ Redis.withMasterRedis $ Redis.get key
      case mbRouteInfo of
        Just routeInfo -> do
          let src = (NE.head req.waypoints) :: Maps.LatLong
          let points = fromMaybe [] routeInfo.points
          selectedWaypoints <- if rideHasStops then pure [] else getSelectedWaypoints src points -- picks 5 points from the sreq ride route, skip if ride has stops
          let req' :: Maps.GetRoutesReq =
                Maps.GetRoutesReq
                  { waypoints = NE.fromList ([src] ++ selectedWaypoints ++ [NE.last req.waypoints]), -- this is done to ensure same route is shown to driver as was in sreq
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

-- | Turn-by-turn navigation routes for the driver app's in-app navigation.
--
-- The @/route@, @/pickup/route@ and @/trip/route@ APIs above convert OSRM's answer into
-- 'Maps.GetRoutesResp', which keeps only the route points and totals: the turn type, direction,
-- road names and per-step distances that navigation needs are dropped. @/navigation/route@ calls this
-- city's OSRM server directly and returns 'OSRM.NavigationRouteResp': every step, with only the fields
-- the app's navigation engine reads, under OSRM's own names.

-- | Route from the driver's position through the remaining stops; the last waypoint is the destination.
data NavigationRouteReq = NavigationRouteReq
  { origin :: Maps.LatLong,
    -- | The driver's heading in degrees, when known. OSRM then starts the route in that direction,
    -- which avoids a route that begins with a U-turn behind the driver after a reroute.
    bearing :: Maybe Double,
    waypoints :: [Maps.LatLong],
    avoidTolls :: Maybe Bool,
    -- | For logging only: ties the call to a ride.
    rideId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Returns this city's OSRM route for the request. @followsSearchRoute@ is true only when the route was
-- guided along the route quoted at search (see 'searchRouteGuidePoints').
--
-- Rate limited per driver as a safety net: the app already asks at most once per reroute, and
-- reroutes are at least 8 s apart. Calling OSRM, the toll fallback, the guide points' fallback and the
-- flags live in 'OSRM.getOsrmNavigationRoute'.
getNavigationRoute ::
  ServiceFlow m r =>
  (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  NavigationRouteReq ->
  m OSRM.NavigationRouteResp
getNavigationRoute (personId, _merchantId, merchantOpCityId) req = do
  when (null req.waypoints) $ throwError $ InvalidRequest "waypoints must not be empty"
  checkSlidingWindowLimitWithOptions (navigationRouteHitsKey personId) navigationRouteRateLimitOptions
  osrmCfg <- Maps.getNavigationRoutingConfig merchantOpCityId
  guidePoints <- searchRouteGuidePoints personId req
  route <-
    OSRM.getOsrmNavigationRoute
      (req.rideId <|> Just personId.getId)
      (Just merchantOpCityId.getId)
      osrmCfg.osrmUrl
      Maps.CAR
      req.origin
      req.waypoints
      OSRM.NavigationRouteOptions {originBearing = req.bearing, avoidTolls = fromMaybe False req.avoidTolls, viaPoints = guidePoints}
  unless (null guidePoints) $ logInfo $ "Navigation route guided along the search route: " <> routeLogSummary route
  pure route

-- | Points of the route quoted to the customer at search (cached in Redis for an hour) to guide the drop
-- leg along, so the driver takes the same roads as the quote and the distance driven, and with it the
-- fare, stays close to it. The same points 'getTripRoutes' guides its route with.
--
-- Empty, so that the route is made without guidance, unless the driver has a ride in progress without
-- stops, the request is for a single destination, the cached route is still there, and the driver is at
-- its start. The cached route ends at the destination, which is left out: a guide point on the
-- destination could snap to the other side of its road and add a loop at the end.
searchRouteGuidePoints :: ServiceFlow m r => Id DP.Person -> NavigationRouteReq -> m [Maps.LatLong]
searchRouteGuidePoints personId req =
  QRide.getInProgressByDriverId personId >>= \case
    Just ride | not (fromMaybe False ride.hasStops),
                [_] <- req.waypoints -> do
      mbBooking <- QBookingLite.findByIdLite ride.bookingId
      mbRouteInfo :: Maybe RI.RouteInfo <-
        join <$> forM mbBooking (\booking -> Redis.runInMultiCloudRedisMaybeResult $ Redis.withMasterRedis $ Redis.get (searchRequestKey booking.transactionId))
      case mbRouteInfo >>= (.points) of
        Just searchRoutePoints -> withoutLast <$> getSelectedWaypoints req.origin searchRoutePoints
        Nothing -> pure []
    _ -> pure []
  where
    withoutLast guidePoints = take (length guidePoints - 1) guidePoints

-- | The length of a navigation route and whether it follows the search route, for the log.
routeLogSummary :: OSRM.NavigationRouteResp -> Text
routeLogSummary route =
  "followsSearchRoute=" <> show route.followsSearchRoute <> " osrmDistanceMeters=" <> show ((.distance) <$> listToMaybe route.routes)

-- | Route requests allowed per driver: 10 a minute, above the most a driver can need (the app reroutes
-- at most once every 8 s).
navigationRouteRateLimitOptions :: APIRateLimitOptions
navigationRouteRateLimitOptions = APIRateLimitOptions {limit = 10, limitResetTimeInSec = 60}

navigationRouteHitsKey :: Id DP.Person -> Text
navigationRouteHitsKey personId = "NavigationRouteHits:" <> personId.getId <> ":hitsCount"
