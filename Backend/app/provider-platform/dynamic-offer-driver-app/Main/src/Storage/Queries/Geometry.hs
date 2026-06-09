{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Geometry where

import Domain.Types.Geometry
import Kernel.Beam.Functions
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.GateInfo.Geometry as GGeom
import qualified Sequelize as Se
import qualified Storage.Beam.Geometry as BeamG
import qualified Storage.Beam.Geometry.GeometryGeom as BeamGeomG

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Geometry -> m ()
create g = do
  createWithKV g
  clearGeometryInMemCache

-- | Drop the geometry L1 in-mem cache and propagate the cleanup to other pods. Call
--   after any geometry create/update.
clearGeometryInMemCache :: (MonadFlow m, CacheFlow m r) => m ()
clearGeometryInMemCache = IM.refreshInMem "Geometry:"

-- | Single source of truth for geometry reads: every geometry (city/region boundary)
--   paired with its parsed polygon(s) (empty when no geom), held in an in-memory cache
--   (1h TTL). The table is small and static, so all geometry lookups below are served
--   from this cache — no DB/PostGIS on the read path.
getAllGeometriesWithPolygons :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [(Geometry, [[[LatLong]]])]
getAllGeometriesWithPolygons =
  IM.withInMemCache ["Geometry:All"] 3600 $ do
    geoms <- findAllWithKV [Se.Is BeamG.id $ Se.Not (Se.Eq "")]
    pure $ map (\g -> (g, maybe [] (fromMaybe [] . GGeom.parseGatePolygons) g.geom)) geoms

findGeometryByStateAndCity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Context.IndianState -> m (Maybe Geometry)
findGeometryByStateAndCity cityParam stateParam = do
  cached <- getAllGeometriesWithPolygons
  pure $ fst <$> find (\(g, _) -> g.city == cityParam && g.state == stateParam) cached

findGeometriesContaining :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions = do
  cached <- getAllGeometriesWithPolygons
  pure [g | (g, polys) <- cached, g.region `elem` regions, any (GGeom.pointInPolygon gps) polys]

someGeometriesContain :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> [Text] -> m Bool
someGeometriesContain gps regions = not . null <$> findGeometriesContaining gps regions

findGeometriesContainingGps :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> m [Geometry]
findGeometriesContainingGps gps = do
  cached <- getAllGeometriesWithPolygons
  pure [g | (g, polys) <- cached, any (GGeom.pointInPolygon gps) polys]

instance FromTType' BeamG.Geometry Geometry where
  fromTType' BeamG.GeometryT {..} = do
    pure $
      Just
        Geometry
          { id = Id id,
            geom = geomGeoJson,
            ..
          }

instance ToTType' BeamGeomG.GeometryGeom Geometry where
  toTType' Geometry {..} = do
    BeamGeomG.GeometryGeomT
      { BeamGeomG.id = getId id,
        BeamGeomG.geom = Nothing,
        BeamGeomG.geomGeoJson = geom,
        ..
      }
