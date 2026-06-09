{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Queries.GateInfoExtra where

import Data.List (sortBy)
import Data.Ord (comparing)
import Kernel.Beam.Functions
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Lib.GateInfo.Geometry (parseGatePolygons, pointInPolygon)
import Lib.Queries.OrphanInstances.GateInfo ()
import qualified Lib.Queries.Transformers.GateInfo as TGI
import Lib.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Storage.Beam.GateInfo as Beam
import qualified Lib.Types.GateInfo as D
import qualified Lib.Types.SpecialLocation as SL
import qualified Sequelize as Se

-- Extra code goes here --

-- | Single source of truth for gate reads: every gate paired with its parsed
--   pickup-zone polygon(s) (empty when the gate has no geom), held in an in-memory
--   cache (1h TTL). Gate rows are admin-edited and rarely change, and the table is
--   small, so all gate lookups below are served from this cache — no DB/PostGIS on the
--   read path. Invalidated on any gate/special-location write via
--   'Lib.Queries.SpecialLocation.clearSpecialZoneInMemCache' (refreshes the "GateInfo:"
--   key prefix across pods).
getAllGatesCached :: (BeamFlow m r) => m [(D.GateInfo, [[[LatLong]]])]
getAllGatesCached =
  IM.withInMemCache ["GateInfo:All"] 3600 $ do
    gates <- findAllWithKV [Se.Is Beam.id $ Se.Not (Se.Eq "")]
    pure $ map (\g -> (g, maybe [] (fromMaybe [] . parseGatePolygons) g.geomGeoJson)) gates

gatesAtSpecialLocation :: (BeamFlow m r) => Id SL.SpecialLocation -> m [(D.GateInfo, [[[LatLong]]])]
gatesAtSpecialLocation slId = filter (\(g, _) -> g.specialLocationId == slId) <$> getAllGatesCached

-- | All gates of a special location, paired with their GeoJSON polygon text. Kept
--   tuple-shaped for backward compatibility with callers that used the old PostGIS
--   @ST_AsGeoJSON@ projection. Served from the in-memory cache.
findAllGatesBySpecialLocationId :: (BeamFlow m r) => Id SL.SpecialLocation -> m [(D.GateInfo, Maybe Text)]
findAllGatesBySpecialLocationId slId = map (\(g, _) -> (g, g.geomGeoJson)) <$> gatesAtSpecialLocation slId

findAllGatesBySpecialLocationIdWithoutGeoJson :: (BeamFlow m r) => Id SL.SpecialLocation -> m [D.GateInfo]
findAllGatesBySpecialLocationIdWithoutGeoJson slId = map fst <$> gatesAtSpecialLocation slId

-- | Find the gate whose pickup-zone polygon contains the given point. Pure Haskell
--   point-in-polygon over the in-memory cached polygons — no PostGIS.
findGateInfoIfDriverInsideGatePickupZone :: (BeamFlow m r) => LatLong -> m (Maybe D.GateInfo)
findGateInfoIfDriverInsideGatePickupZone point = do
  gates <- getAllGatesCached
  pure $ fst <$> find (\(_, polys) -> any (pointInPolygon point) polys) gates

-- | Exact match on the gate's point. Served from the in-memory cache.
findGateInfoByLatLongWithoutGeoJson :: (BeamFlow m r) => LatLong -> m (Maybe D.GateInfo)
findGateInfoByLatLongWithoutGeoJson point = do
  gates <- getAllGatesCached
  pure $ fst <$> find (\(g, _) -> g.point == point) gates

-- | Find the nearest gate within a given radius (meters) of the given point, scoped to
--   a special location. Haversine distance over the in-memory cache; tie-breaks by gate
--   id for deterministic selection.
findGateInfoByLatLongWithinRadius :: (BeamFlow m r) => Id SL.SpecialLocation -> LatLong -> Double -> m (Maybe D.GateInfo)
findGateInfoByLatLongWithinRadius slId point radiusMeters = do
  gates <- map fst <$> gatesAtSpecialLocation slId
  let gatesWithDist = map (\g -> (distanceBetweenInMeters point g.point, g)) gates
      gatesInRadius = filter (\(d, _) -> d <= realToFrac radiusMeters) gatesWithDist
      sorted = sortBy (comparing (\(d, g) -> (d, g.id))) gatesInRadius
  pure $ snd <$> listToMaybe sorted

findGatesWithDriverThreshold :: (BeamFlow m r) => Id SL.SpecialLocation -> m [D.GateInfo]
findGatesWithDriverThreshold slId = do
  gates <- map fst <$> gatesAtSpecialLocation slId
  pure $ filter (\g -> g.canQueueUpOnGate && (isJust g.minDriverThresholds || isJust g.defaultMinDriverThreshold)) gates

deleteById :: (BeamFlow m r) => Id D.GateInfo -> m ()
deleteById gateId = deleteWithKV [Se.Is Beam.id $ Se.Eq (getId gateId)]

deleteAll :: (BeamFlow m r) => Id SL.SpecialLocation -> m ()
deleteAll slId = deleteWithKV [Se.Is Beam.specialLocationId $ Se.Eq (getId slId)]

-- | Replace the gate matched by (specialLocationId, name). Writes the pickup-zone
--   polygon as GeoJSON text into @geom_geojson@ (no PostGIS geometry).
updateGate :: (BeamFlow m r) => D.GateInfo -> m ()
updateGate gate =
  updateWithKV
    [ Se.Set Beam.name gate.name,
      Se.Set Beam.point (TGI.latLongToText gate.point),
      Se.Set Beam.defaultDriverExtra gate.defaultDriverExtra,
      Se.Set Beam.address gate.address,
      Se.Set Beam.merchantOperatingCityId (getId <$> gate.merchantOperatingCityId),
      Se.Set Beam.merchantId (getId <$> gate.merchantId),
      Se.Set Beam.updatedAt gate.updatedAt,
      Se.Set Beam.canQueueUpOnGate gate.canQueueUpOnGate,
      Se.Set Beam.geomGeoJson gate.geomGeoJson,
      Se.Set Beam.gateTags gate.gateTags,
      Se.Set Beam.walkDescription gate.walkDescription
    ]
    [Se.And [Se.Is Beam.specialLocationId $ Se.Eq (getId gate.specialLocationId), Se.Is Beam.name $ Se.Eq gate.name]]
