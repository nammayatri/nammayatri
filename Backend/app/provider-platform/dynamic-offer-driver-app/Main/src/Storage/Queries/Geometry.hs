{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Geometry where

import Domain.Types.Geometry
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Geometry as BeamG
import Storage.Tabular.Geometry

findGeometriesContaining :: Transactionable m => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions =
  Esq.findAll $ do
    geometry <- from $ table @GeometryT
    where_ $
      geometry ^. GeometryRegion `in_` valList regions
        &&. containsPoint (gps.lon, gps.lat)
    return geometry

someGeometriesContain :: Transactionable m => LatLong -> [Text] -> m Bool
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
  BeamG.defaultGeometry
    { BeamG.id = getId id,
      BeamG.region = region
    }
