{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can Hedistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BlockedRouteDetector where

import Domain.Types.BlockedRoute
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.ComputeIntersection
import Storage.CachedQueries.BlockedRoute (findAllBlockedRoutesByMerchantOperatingCity)

-- If given route points intersect with any of the start & end segments of blocked routes, then we mark the route as unserviceable by returning `False`
isRouteServiceable :: RoutePoints -> [BlockedRoute] -> Bool
isRouteServiceable routePoints blockedRoutes = do
  let boundingBox = getBoundingBox routePoints
      eligibleStartBlockedRouteSegmentsPresentOnRoute = filter (\blockedRoute -> lineSegmentWithinBoundingBox blockedRoute.startSegment boundingBox) blockedRoutes
  getRouteServiceable routePoints eligibleStartBlockedRouteSegmentsPresentOnRoute
  where
    getRouteServiceable [] _ = True
    getRouteServiceable [_] _ = True
    getRouteServiceable route@(r1 : r2 : rs) eligibleBlockedRoutes = do
      let allBlockedRoutesStartCombinations = filter (\blockedRoute -> doIntersect (LineSegment r1 r2) blockedRoute.startSegment) eligibleBlockedRoutes
      if null allBlockedRoutesStartCombinations || (not $ doEndSegmentOfAnyStartBlockedRouteIntersectRoute route allBlockedRoutesStartCombinations)
        then getRouteServiceable (r2 : rs) eligibleBlockedRoutes
        else False

    doEndSegmentOfAnyStartBlockedRouteIntersectRoute [] _ = False
    doEndSegmentOfAnyStartBlockedRouteIntersectRoute [_] _ = False
    doEndSegmentOfAnyStartBlockedRouteIntersectRoute _ [] = False
    doEndSegmentOfAnyStartBlockedRouteIntersectRoute (r1 : r2 : rs) eligibleBlockedRoutes = do
      if isJust $ find (\blockedRoute -> doIntersect (LineSegment r1 r2) blockedRoute.endSegment) eligibleBlockedRoutes
        then True
        else doEndSegmentOfAnyStartBlockedRouteIntersectRoute (r2 : rs) eligibleBlockedRoutes

data RouteServiceability = RouteServiceability
  { isCustomerPrefferedSearchRoute :: Bool,
    isBlockedRoute :: Bool,
    multipleRoutes :: [Maps.RouteInfo],
    routePoints :: RoutePoints,
    routeDistance :: Meters,
    routeDuration :: Seconds
  }

checkRouteServiceability :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> (Int, RoutePoints, Meters, Seconds) -> [Maps.RouteInfo] -> m RouteServiceability
checkRouteServiceability merchantOperatingCityId (customerPrefferedSearchRouteIdx, customerPrefferedSearchRoutePoints, customerPrefferedSearchRouteDistance, customerPrefferedSearchRouteDuration) routes = do
  let route' =
        if null routes
          then [Maps.RouteInfo (Just customerPrefferedSearchRouteDuration) Nothing (Just customerPrefferedSearchRouteDistance) (Just $ Distance (toHighPrecDistance customerPrefferedSearchRouteDistance) Meter) Nothing [] customerPrefferedSearchRoutePoints]
          else routes
  blockedRoutes <- B.runInReplica $ findAllBlockedRoutesByMerchantOperatingCity merchantOperatingCityId
  return $ getServiceableRoute route' 0 blockedRoutes
  where
    getServiceableRoute _ _ [] = defaultCustomerPrefferedRouteServiceability False
    getServiceableRoute [] _ _ = defaultCustomerPrefferedRouteServiceability True
    getServiceableRoute ((Maps.RouteInfo (Just duration) _ (Just distance) _ _ _ points) : rs) idx blockedRoutes = do
      if isRouteServiceable points blockedRoutes
        then
          RouteServiceability
            { routePoints = points,
              routeDistance = distance,
              routeDuration = duration,
              multipleRoutes = updateEfficientRoutePosition routes idx,
              isCustomerPrefferedSearchRoute = idx == customerPrefferedSearchRouteIdx,
              isBlockedRoute = False
            }
        else getServiceableRoute rs (idx + 1) blockedRoutes
    getServiceableRoute (_ : rs) idx blockedRoutes = getServiceableRoute rs (idx + 1) blockedRoutes

    updateEfficientRoutePosition routeInfos idx = do
      let (x, y) = splitAt idx routeInfos
      y ++ x

    defaultCustomerPrefferedRouteServiceability isBlockedRoute =
      RouteServiceability
        { routePoints = customerPrefferedSearchRoutePoints,
          routeDistance = customerPrefferedSearchRouteDistance,
          routeDuration = customerPrefferedSearchRouteDuration,
          multipleRoutes = routes,
          isCustomerPrefferedSearchRoute = True,
          isBlockedRoute
        }
