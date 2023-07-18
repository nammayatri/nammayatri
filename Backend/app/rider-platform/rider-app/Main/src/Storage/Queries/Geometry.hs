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
import Domain.Types.Geometry
import qualified EulerHS.Language as L
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id (Id (..))
import Lib.Utils (getMasterBeamConfig, getReplicaBeamConfig)
import Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Geometry as BeamG

-- findGeometriesContaining :: Transactionable m => LatLong -> [Text] -> m [Geometry]
-- findGeometriesContaining gps regions =
--   Esq.findAll $ do
--     geometry <- from $ table @GeometryT
--     where_ $
--       geometry ^. GeometryRegion `in_` valList regions
--         &&. containsPoint (gps.lon, gps.lat)
--     return geometry

findGeometriesContaining :: L.MonadFlow m => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions = do
  dbConf <- getMasterBeamConfig
  geoms <- L.runDB dbConf $ L.findRows $ B.select $ B.filter_' (\BeamG.GeometryT {..} -> containsPoint' (gps.lon, gps.lat) B.&&?. B.sqlBool_ (region `B.in_` (B.val_ <$> regions))) $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  pure (either (const []) (transformBeamGeometryToDomain <$>) geoms)

-- someGeometriesContain :: Transactionable m => LatLong -> [Text] -> m Bool
-- someGeometriesContain gps regions = do
--   geometries <- findGeometriesContaining gps regions
--   pure $ not $ null geometries

someGeometriesContain :: L.MonadFlow m => LatLong -> [Text] -> m Bool
someGeometriesContain gps regions = do
  geometries <- findGeometriesContaining gps regions
  pure $ not $ null geometries

someGeometriesContainInReplica :: L.MonadFlow m => LatLong -> [Text] -> m Bool
someGeometriesContainInReplica gps regions = do
  dbConf <- getReplicaBeamConfig
  geoms <- L.runDB dbConf $ L.findRows $ B.select $ B.filter_' (\BeamG.GeometryT {..} -> containsPoint' (gps.lon, gps.lat) B.&&?. B.sqlBool_ (region `B.in_` (B.val_ <$> regions))) $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  pure $ either (const False) (not . null) geoms

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
