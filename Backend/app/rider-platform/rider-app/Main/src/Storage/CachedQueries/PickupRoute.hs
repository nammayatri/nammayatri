{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.PickupRoute
  ( cachePickupRoute,
    getPickupRouteFromCache,
    invalidatePickupRouteCache,
  )
where

import Domain.Types.Ride
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

makePickupRouteKey :: Id Ride -> Text
makePickupRouteKey rideId = "CachedQueries:PickupRoute:RideId-" <> rideId.getId

cachePickupRoute ::
  (CacheFlow m r) =>
  Id Ride ->
  [LatLong] ->
  m ()
cachePickupRoute rideId routePoints = do
  let key = makePickupRouteKey rideId
  Hedis.setExp key routePoints 1800 -- 30 minutes TTL

getPickupRouteFromCache ::
  (CacheFlow m r) =>
  Id Ride ->
  m (Maybe [LatLong])
getPickupRouteFromCache rideId = do
  let key = makePickupRouteKey rideId
  Hedis.safeGet key

invalidatePickupRouteCache ::
  (CacheFlow m r) =>
  Id Ride ->
  m ()
invalidatePickupRouteCache rideId = do
  let key = makePickupRouteKey rideId
  Hedis.del key
