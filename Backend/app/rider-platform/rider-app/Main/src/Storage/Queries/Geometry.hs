{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Geometry
  ( create,
    findGeometryByStateAndCity,
    findGeometriesContaining,
    findGeometriesWithinBuffer,
  )
where

import Data.Either
import qualified Data.ByteString.Char8 as BS8
import qualified Database.Beam as B
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Domain.Types.Geometry
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Geometry.Geometry as BeamG
import qualified Storage.Beam.Geometry.GeometryGeom as BeamGeomG

create :: (MonadFlow m, EsqDBFlow m r) => Geometry -> m ()
create = createWithKV

findGeometryByStateAndCity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Context.IndianState -> m (Maybe Geometry)
findGeometryByStateAndCity cityParam stateParam = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamG.city (Se.Eq cityParam),
          Se.Is BeamG.state (Se.Eq stateParam)
        ]
    ]

findGeometriesContaining :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions = do
  dbConf <- getReplicaBeamConfig
  geoms <- L.runDB dbConf $ L.findRows $ B.select $ B.filter_' (\BeamG.GeometryT {..} -> containsPoint' (gps.lon, gps.lat) B.&&?. B.sqlBool_ (region `B.in_` (B.val_ <$> regions))) $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

-- | Find geometries within a buffer distance (in meters) of a point.
-- Uses PostGIS ST_DWithin with geography cast for meter-based distance.
-- Fallback when strict ST_Contains fails due to GPS inaccuracy at polygon edges.
findGeometriesWithinBuffer :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LatLong -> [Text] -> Int -> m [Geometry]
findGeometriesWithinBuffer gps regions bufferMeters = do
  dbConf <- getReplicaBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamG.GeometryT {..} ->
                B.sqlBool_ (dWithinPoint' (gps.lon, gps.lat) bufferMeters)
                  B.&&?. B.sqlBool_ (region `B.in_` (B.val_ <$> regions))
            )
            $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

-- | Raw PostGIS ST_DWithin expression for geography-based distance check.
dWithinPoint' :: (Double, Double) -> Int -> BQ.QGenExpr ctxt Postgres s Bool
dWithinPoint' (lon, lat) bufferM =
  BQ.QExpr (\_ -> PgExpressionSyntax (emit sqlBS))
  where
    sqlBS =
      BS8.pack $
        "ST_DWithin(geom::geography, ST_SetSRID(ST_Point("
          ++ show lon
          ++ ", "
          ++ show lat
          ++ "), 4326)::geography, "
          ++ show bufferM
          ++ ")"

instance FromTType' BeamG.Geometry Geometry where
  fromTType' BeamG.GeometryT {..} = do
    pure $
      Just
        Geometry
          { id = Id id,
            geom = Nothing,
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
