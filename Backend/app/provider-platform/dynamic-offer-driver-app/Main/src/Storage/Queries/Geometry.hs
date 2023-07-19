{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Storage.Queries.Geometry where

import qualified Database.Beam as B
-- import Database.Beam.Postgres
-- import qualified Database.Beam.Schema.Tables as B
import Domain.Types.Geometry
-- import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils
import qualified Storage.Beam.Common as BeamCommon
-- import Sequelize
import qualified Storage.Beam.Geometry as BeamG

-- data AtlasDB f = AtlasDB
--   { geometry :: f (B.TableEntity BeamG.GeometryT)
--   }
--   deriving (Generic, B.Database be)

-- atlasDB :: B.DatabaseSettings be AtlasDB
-- atlasDB =
--   B.defaultDbSettings
--     `B.withDbModification` B.dbModification
--       { geometry = geometryTable
--       }

-- geometryTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity BeamG.GeometryT)
-- geometryTable =
--   B.setEntitySchema (Just "atlas_driver_offer_bpp")
--     <> B.setEntityName "geometry"
--     <> B.modifyTableFields BeamG.geometryTMod

-- findGeometriesContaining :: Transactionable m => LatLong -> [Text] -> m [Geometry]
-- findGeometriesContaining gps regions =
--   Esq.findAll $ do
--     geometry <- from $ table @GeometryT
--     where_ $
--       geometry ^. GeometryRegion `in_` valList regions
--         &&. containsPoint (gps.lon, gps.lat) --QExpr (\tbl -> PgCommandSyntax PgCommandTypeQuery (emit $ "st_contains (" <> show gps.lon <> " , " <> show gps.lat <> ")"))
--         -- containsPoint (gps.lon, gps.lat)
--     return geometry

findGeometriesContaining :: forall m. (L.MonadFlow m) => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions = do
  dbConf <- getMasterBeamConfig
  geoms <- L.runDB dbConf $ L.findRows $ B.select $ B.filter_' (\BeamG.GeometryT {..} -> containsPoint' (gps.lon, gps.lat) B.&&?. B.sqlBool_ (region `B.in_` (B.val_ <$> regions))) $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  -- geoms <- L.runDB c $ L.findRows $ B.select $ B.filter_' (\BeamG.GeometryT {..} -> B.sqlBool_ (region `B.in_` (B.val_ <$> regions))) $ B.all_ (geometry atlasDB)
  pure (either (const []) (transformBeamGeometryToDomain <$>) geoms)

someGeometriesContain :: forall m. (L.MonadFlow m) => LatLong -> [Text] -> m Bool
someGeometriesContain gps regions = do
  geometries <- findGeometriesContaining gps regions
  pure $ not $ null geometries

transformBeamGeometryToDomain :: BeamG.Geometry -> Geometry
transformBeamGeometryToDomain BeamG.GeometryT {..} = do
  Geometry
    { id = Id id,
      region = region
    }

transformDomainGeometryToBeam :: Geometry -> BeamG.Geometry
transformDomainGeometryToBeam Geometry {..} =
  BeamG.GeometryT
    { BeamG.id = getId id,
      BeamG.region = region
    }
