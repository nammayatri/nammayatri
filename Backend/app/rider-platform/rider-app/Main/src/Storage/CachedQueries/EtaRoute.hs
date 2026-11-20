{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.EtaRoute
  ( EtaLeg (..),
    EtaBasis (..),
    cacheRoute,
    getRouteFromCache,
    invalidateRouteCache,
    isRefreshDue,
    markRefreshed,
    cacheEtaBasis,
    getEtaBasis,
    isBasisTargetStale,
  )
where

import Domain.Types.Ride
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common

data EtaLeg = PickupLeg | DropLeg
  deriving (Eq, Show)

routeTtlSeconds :: Int
routeTtlSeconds = 1800

defaultRefreshIntervalSeconds :: Int
defaultRefreshIntervalSeconds = 60

makeRouteKey :: EtaLeg -> Id Ride -> Text
makeRouteKey PickupLeg rideId = "CachedQueries:PickupRoute:RideId-" <> rideId.getId
makeRouteKey DropLeg rideId = "CachedQueries:DropRoute:RideId-" <> rideId.getId

makeRefreshKey :: EtaLeg -> Id Ride -> Text
makeRefreshKey PickupLeg rideId = "CachedQueries:PickupSpeedRefresh:RideId-" <> rideId.getId
makeRefreshKey DropLeg rideId = "CachedQueries:DropEtaRefresh:RideId-" <> rideId.getId

makeEtaBasisKey :: EtaLeg -> Id Ride -> Text
makeEtaBasisKey PickupLeg rideId = "CachedQueries:PickupEtaBasis:RideId-" <> rideId.getId
makeEtaBasisKey DropLeg rideId = "CachedQueries:DropEtaBasis:RideId-" <> rideId.getId

cacheRoute ::
  (CacheFlow m r) =>
  EtaLeg ->
  Id Ride ->
  [LatLong] ->
  m ()
cacheRoute leg rideId routePoints = Hedis.setExp (makeRouteKey leg rideId) routePoints routeTtlSeconds

getRouteFromCache ::
  (CacheFlow m r) =>
  EtaLeg ->
  Id Ride ->
  m (Maybe [LatLong])
getRouteFromCache leg rideId = Hedis.safeGet (makeRouteKey leg rideId)

invalidateRouteCache ::
  (CacheFlow m r) =>
  EtaLeg ->
  Id Ride ->
  m ()
invalidateRouteCache leg rideId = Hedis.del (makeRouteKey leg rideId)

isRefreshDue :: (CacheFlow m r) => EtaLeg -> Id Ride -> m Bool
isRefreshDue leg rideId = do
  mbMarker <- Hedis.safeGet (makeRefreshKey leg rideId)
  pure $ isNothing (mbMarker :: Maybe Bool)

markRefreshed :: (CacheFlow m r) => EtaLeg -> Id Ride -> Maybe Seconds -> m ()
markRefreshed leg rideId mbInterval =
  Hedis.setExp (makeRefreshKey leg rideId) True $
    maybe defaultRefreshIntervalSeconds (.getSeconds) mbInterval

data EtaBasis = EtaBasis
  { durationSeconds :: Seconds,
    computedAt :: UTCTime,
    target :: Maybe LatLong -- destination latlon
  }
  deriving (Generic, Show, FromJSON, ToJSON)

cacheEtaBasis :: (CacheFlow m r) => EtaLeg -> Id Ride -> Seconds -> UTCTime -> Maybe LatLong -> m ()
cacheEtaBasis leg rideId durationSeconds computedAt target =
  Hedis.setExp (makeEtaBasisKey leg rideId) EtaBasis {..} routeTtlSeconds

getEtaBasis :: (CacheFlow m r) => EtaLeg -> Id Ride -> m (Maybe EtaBasis)
getEtaBasis leg rideId = Hedis.safeGet (makeEtaBasisKey leg rideId)

targetMovedThresholdInMeters :: Double
targetMovedThresholdInMeters = 20

-- | A drop ETA must not keep counting down to a destination the rider has edited.
isBasisTargetStale :: LatLong -> EtaBasis -> Bool
isBasisTargetStale currentTarget basis =
  case basis.target of
    Nothing -> False
    Just cachedTarget ->
      realToFrac (getHighPrecMeters (CD.distanceBetweenInMeters cachedTarget currentTarget)) > targetMovedThresholdInMeters
