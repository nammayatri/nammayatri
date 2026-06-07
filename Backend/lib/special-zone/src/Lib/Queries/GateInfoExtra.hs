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
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
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

-- | All gates of a special location, paired with their raw GeoJSON polygon text
--   (from the @geom_geojson@ column). Kept tuple-shaped for backward compatibility
--   with callers that used the old PostGIS @ST_AsGeoJSON@ projection.
findAllGatesBySpecialLocationId :: (BeamFlow m r) => Id SL.SpecialLocation -> m [(D.GateInfo, Maybe Text)]
findAllGatesBySpecialLocationId slId =
  map (\g -> (g, g.geomGeoJson)) <$> findAllGatesBySpecialLocationIdWithoutGeoJson slId

findAllGatesBySpecialLocationIdWithoutGeoJson :: (BeamFlow m r) => Id SL.SpecialLocation -> m [D.GateInfo]
findAllGatesBySpecialLocationIdWithoutGeoJson slId =
  findAllWithKV [Se.Is Beam.specialLocationId $ Se.Eq (getId slId)]

-- | Cache entry for the global "which gate contains this point" lookup. Carries the
--   gate plus its parsed pickup-zone polygons so containment runs purely in Haskell.
data GateWithPolygons = GateWithPolygons
  { gate :: D.GateInfo,
    polygons :: [[[LatLong]]]
  }
  deriving (Show, Generic, ToJSON, Typeable)

-- | All gates that have a pickup-zone polygon, with polygons parsed once and held
--   in an in-memory cache (1-hour TTL). Gate polygons are admin-edited and rarely
--   change, so a stale read is acceptable; this replaces the per-call PostGIS
--   @ST_Contains@ scan that the old query did on the hot path.
getAllGatesWithPolygonsCached :: (BeamFlow m r, CoreMetrics m) => m [GateWithPolygons]
getAllGatesWithPolygonsCached =
  IM.withInMemCache ["GateInfo:AllWithGeom"] 3600 $ do
    gates <- findAllWithKV [Se.Is Beam.geomGeoJson $ Se.Not (Se.Eq Nothing)]
    pure $
      map
        (\g -> GateWithPolygons {gate = g, polygons = fromMaybe [] (g.geomGeoJson >>= parseGatePolygons)})
        gates

-- | Find the gate whose pickup-zone polygon contains the given point. Pure Haskell
--   point-in-polygon over the in-memory cached polygons — no PostGIS.
findGateInfoIfDriverInsideGatePickupZone :: (BeamFlow m r, CoreMetrics m) => LatLong -> m (Maybe D.GateInfo)
findGateInfoIfDriverInsideGatePickupZone point = do
  gates <- getAllGatesWithPolygonsCached
  pure $ (.gate) <$> find (\gp -> any (pointInPolygon point) gp.polygons) gates

-- | Exact match on the serialized @point@ column.
findGateInfoByLatLongWithoutGeoJson :: (BeamFlow m r) => LatLong -> m (Maybe D.GateInfo)
findGateInfoByLatLongWithoutGeoJson point =
  findOneWithKV [Se.Is Beam.point $ Se.Eq (TGI.latLongToText point)]

-- | Find the nearest gate within a given radius (meters) of the given point,
--   scoped to a special location. Uses Haversine distance since the @point@ column
--   is a serialized LatLong, not PostGIS geometry. Tie-breaks by gate id so the
--   selection is deterministic.
findGateInfoByLatLongWithinRadius :: (BeamFlow m r) => Id SL.SpecialLocation -> LatLong -> Double -> m (Maybe D.GateInfo)
findGateInfoByLatLongWithinRadius slId point radiusMeters = do
  gates <- findAllGatesBySpecialLocationIdWithoutGeoJson slId
  let gatesWithDist = map (\g -> (distanceBetweenInMeters point g.point, g)) gates
      gatesInRadius = filter (\(d, _) -> d <= realToFrac radiusMeters) gatesWithDist
      sorted = sortBy (comparing (\(d, g) -> (d, g.id))) gatesInRadius
  pure $ snd <$> listToMaybe sorted

findGatesWithDriverThreshold :: (BeamFlow m r) => Id SL.SpecialLocation -> m [D.GateInfo]
findGatesWithDriverThreshold slId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.specialLocationId $ Se.Eq (getId slId),
          Se.Is Beam.canQueueUpOnGate $ Se.Eq True,
          Se.Or
            [ Se.Is Beam.minDriverThresholdsJson $ Se.Not (Se.Eq Nothing),
              Se.Is Beam.defaultMinDriverThreshold $ Se.Not (Se.Eq Nothing)
            ]
        ]
    ]

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
