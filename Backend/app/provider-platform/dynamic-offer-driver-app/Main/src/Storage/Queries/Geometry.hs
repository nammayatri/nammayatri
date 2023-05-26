{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Geometry where

import qualified Database.Beam as B
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import qualified Database.Beam.Schema.Tables as B
import Domain.Types.Geometry
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import EulerHS.Prelude (appEndo)
import qualified EulerHS.SqlDB.Language as DB
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import Sequelize
import qualified Storage.Beam.Geometry as BeamG
import Storage.Tabular.Geometry

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
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      geoms <- L.runDB c $ L.findRows $ B.select $ B.filter_' (\BeamG.GeometryT {..} -> containsPoint' (gps.lat, gps.lon) B.&&?. B.sqlBool_ (region `B.in_` (B.val_ <$> regions))) $ B.all_ (meshModelTableEntity @BeamG.GeometryT @Postgres @(DatabaseWith BeamG.GeometryT))
      pure (either (const []) (transformBeamGeometryToDomain <$>) geoms)
    Left _ -> pure []

-- meshModelTableEntity ::
--   (B.Database be db, be ~ Postgres, Model Postgres BeamG.GeometryT, MeshMeta Postgres BeamG.GeometryT) =>
--   B.DatabaseEntity Postgres db (B.TableEntity BeamG.GeometryT)
-- meshModelTableEntity =
--   -- let B.EntityModification modification = B.modifyTableFields (meshModelFieldModification @table)
--   let B.EntityModification modification =
--         B.modifyTableFields BeamG.geometryTMod
--           <> B.setEntityName "geometry"
--   --in appEndo modification $ B.DatabaseEntity $ B.dbEntityAuto (modelTableName @table)
--   in appEndo modification $ B.DatabaseEntity $ B.dbEntityAuto "geometry"

-- modelTableEntity' ::
--   forall table be db.
--   Model be table =>
--   B.DatabaseEntity be db (B.TableEntity table)
-- modelTableEntity' =
--   let B.EntityModification modification =
--         B.modifyTableFields (modelFieldModification @table)
--           <> B.setEntityName (modelTableName @table)
--           <> B.setEntitySchema (modelSchemaName @table)
--    in appEndo modification $ B.DatabaseEntity $ B.dbEntityAuto (modelTableName @table)
-- geometryEMod :: B.EntityModification
--   (B.DatabaseEntity Postgres db) be (B.TableEntity GeometryT)
-- geometryEMod = B.modifyTableFields geometryTMod

-- membersEMod :: B.EntityModification
--   (B.DatabaseEntity be db) be (B.TableEntity MemberT)
-- membersEMod = B.modifyTableFields
--   B.tableModification
--     { memberId = B.fieldNamed "memid"
--     , surName = B.fieldNamed "surname"
--     , firstName = B.fieldNamed "firstname"
--     , address = B.fieldNamed "address"
--     , zipCode = B.fieldNamed "zipcode"
--     , telephone = B.fieldNamed "telephone"
--     , recommendedBy = B.fieldNamed "recommendedby"
--     , joinDate = B.fieldNamed "joindate"
--     }

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

containsPoint' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (" <> show lon <> " , " <> show lat <> ")")))
