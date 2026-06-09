{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.GeometryGeom
  ( findAllGeometries,
    findAllGeometriesForMerchant,
    updateGeometry,
  )
where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import qualified Database.Beam as B
import Domain.Types.Geometry
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Geometry.GeometryGeom as BeamGeomG
import Storage.Queries.Geometry (clearGeometryInMemCache, getAllGeometriesWithPolygons)

-- | Geometries for a city, served from the in-memory cache (no PostGIS / DB read),
--   ordered by id descending and paginated.
findAllGeometries :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Maybe Int -> Maybe Int -> m [Geometry]
findAllGeometries cityParam mbLimit mbOffset = do
  cached <- getAllGeometriesWithPolygons
  pure $ paginate mbLimit mbOffset $ map fst $ filter (\(g, _) -> g.city == cityParam) cached

-- | All geometries across cities (allCities=true dashboard listing), from the cache.
findAllGeometriesForMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id merchant -> Maybe Int -> Maybe Int -> m [Geometry]
findAllGeometriesForMerchant _merchantId mbLimit mbOffset = do
  cached <- getAllGeometriesWithPolygons
  pure $ paginate mbLimit mbOffset $ map fst cached

paginate :: Maybe Int -> Maybe Int -> [Geometry] -> [Geometry]
paginate mbLimit mbOffset =
  take (fromMaybe 100 mbLimit) . drop (fromMaybe 0 mbOffset) . sortBy (comparing (Down . (.id)))

-- | Update a geometry row's polygon. Writes the GeoJSON text to geom_geo_json (used by
--   the in-Haskell read path) and, for rollback safety, keeps writing the legacy PostGIS
--   geom column. Invalidates the in-mem geometry cache afterwards.
updateGeometry :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Context.IndianState -> Text -> Text -> Maybe Text -> m ()
updateGeometry cityParam stateParam regionParam newGeom mbNewGeomGeoJson = do
  dbConf <- getMasterBeamConfig
  void $
    L.runDB dbConf $
      L.updateRows $
        B.update'
          (BeamCommon.geometryGeom BeamCommon.atlasDB)
          ( \BeamGeomG.GeometryGeomT {..} ->
              (geom B.<-. B.val_ (Just newGeom)) <> (geomGeoJson B.<-. B.val_ mbNewGeomGeoJson)
          )
          (\BeamGeomG.GeometryGeomT {..} -> B.sqlBool_ $ city B.==. B.val_ cityParam B.&&. state B.==. B.val_ stateParam B.&&. region B.==. B.val_ regionParam)
  clearGeometryInMemCache
