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

import Data.Either
import qualified Database.Beam as B
import Domain.Types.Geometry
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Geometry as BeamG
import qualified Storage.Beam.Geometry.GeometryGeom as BeamGeomG

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Geometry -> m ()
create = createWithKV

findGeometryByStateAndCity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Context.IndianState -> m (Maybe Geometry)
findGeometryByStateAndCity cityParam stateParam = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamG.city (Se.Eq cityParam),
          Se.Is BeamG.state (Se.Eq stateParam)
        ]
    ]

findGeometriesContaining :: forall m r. (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions = do
  dbConf <- getReplicaBeamConfig
  geoms <- L.runDB dbConf $ L.findRows $ B.select $ B.filter_' (\BeamG.GeometryT {..} -> containsPoint' (gps.lon, gps.lat) B.&&?. B.sqlBool_ (region `B.in_` (B.val_ <$> regions))) $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

someGeometriesContain :: forall m r. (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> [Text] -> m Bool
someGeometriesContain gps regions = do
  geometries <- runInReplica $ findGeometriesContaining gps regions
  pure $ not $ null geometries

findGeometriesContainingGps :: forall m r. (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> m [Geometry]
findGeometriesContainingGps gps = do
  dbConf <- getReplicaBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamG.GeometryT {} ->
                containsPoint' (gps.lon, gps.lat)
            )
            $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

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
  toTType' Geometry {..} = do
    BeamGeomG.GeometryGeomT
      { BeamGeomG.id = getId id,
        BeamGeomG.geom = geom,
        ..
      }
