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
    updateGeometry,
  )
where

import Data.Either (fromRight)
import qualified Database.Beam as B
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Domain.Types.Geometry
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Geometry.Geometry as BeamG
import qualified Storage.Beam.Geometry.GeometryGeom as BeamGeomG

-- | Get geometry as GeoJSON text using ST_AsGeoJSON
-- This converts the PostgreSQL geometry type to a Text representation
getGeomAsGeoJSON :: BQ.QGenExpr context Postgres s (Maybe Text)
getGeomAsGeoJSON = BQ.QExpr (\_ -> PgExpressionSyntax (emit "ST_AsGeoJSON(geom)"))

-- | Find all geometries for a city with the geom column converted to GeoJSON text.
-- Uses a raw Beam query with ST_AsGeoJSON to avoid type mismatch with PostgreSQL geometry type.
findAllGeometries :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Maybe Int -> Maybe Int -> m [Geometry]
findAllGeometries cityParam mbLimit mbOffset = do
  let limitVal = fromMaybe 100 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  result <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limitVal) $
            B.offset_ (fromIntegral offsetVal) $
              B.orderBy_ (\(gId, _, _, _, _) -> B.desc_ gId) $
                fmap
                  ( \BeamG.GeometryT {..} ->
                      (id, city, state, region, getGeomAsGeoJSON)
                  )
                  $ B.filter_' (\BeamG.GeometryT {..} -> B.sqlBool_ (city B.==. B.val_ cityParam)) $
                    B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] result)

-- | FromTType instance for the tuple result from the raw query
instance FromTType' (Text, Context.City, Context.IndianState, Text, Maybe Text) Geometry where
  fromTType' (gId, gCity, gState, gRegion, gGeom) = do
    pure $
      Just
        Geometry
          { id = Id gId,
            city = gCity,
            state = gState,
            region = gRegion,
            geom = gGeom
          }

-- | Update geometry using raw Beam update that doesn't return rows.
-- Uses L.updateRows with B.update' to avoid type mismatch error that occurs with updateWithKV
-- which tries to read back the updated rows containing the geometry column.
updateGeometry :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Context.IndianState -> Text -> Text -> m ()
updateGeometry cityParam stateParam regionParam newGeom = do
  dbConf <- getMasterBeamConfig
  void $
    L.runDB dbConf $
      L.updateRows $
        B.update'
          (BeamCommon.geometryGeom BeamCommon.atlasDB)
          ( \BeamGeomG.GeometryGeomT {..} ->
              geom B.<-. B.val_ (Just newGeom)
          )
          (\BeamGeomG.GeometryGeomT {..} -> B.sqlBool_ $ city B.==. B.val_ cityParam B.&&. state B.==. B.val_ stateParam B.&&. region B.==. B.val_ regionParam)

instance FromTType' BeamGeomG.GeometryGeom Geometry where
  fromTType' BeamGeomG.GeometryGeomT {..} = do
    pure $
      Just
        Geometry
          { id = Id id,
            ..
          }

instance ToTType' BeamGeomG.GeometryGeom Geometry where
  toTType' Geometry {..} =
    BeamGeomG.GeometryGeomT
      { BeamGeomG.id = getId id,
        BeamGeomG.region = region,
        BeamGeomG.state = state,
        BeamGeomG.city = city,
        BeamGeomG.geom = geom
      }
