{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RentalSlab where

import Domain.Types.RentalSlab
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (MeshError (MKeyNotFound), MeshResult)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.RentalSlab as BeamRS

createRentalSlab :: L.MonadFlow m => RentalSlab -> m (MeshResult ())
createRentalSlab rentalSlab = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRS.RentalSlabT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainRentalSlabToBeam rentalSlab)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById' :: (MonadThrow m, Log m, Transactionable m) => Id RentalSlab -> DTypeBuilder m (Maybe RentalSlabT)
-- findById' = Esq.findById'

findById :: (L.MonadFlow m) => Id RentalSlab -> m (Maybe RentalSlab)
findById rentalSlabId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRS.RentalSlabT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      either (pure Nothing) (transformBeamRentalSlabToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamRS.id $ Se.Eq (getId rentalSlabId)]
    Nothing -> pure Nothing

transformBeamRentalSlabToDomain :: BeamRS.RentalSlab -> RentalSlab
transformBeamRentalSlabToDomain BeamRS.RentalSlabT {..} = do
  RentalSlab
    { id = Id id,
      baseDistance = baseDistance,
      baseDuration = baseDuration
    }

transformDomainRentalSlabToBeam :: RentalSlab -> BeamRS.RentalSlab
transformDomainRentalSlabToBeam RentalSlab {..} =
  BeamRS.RentalSlabT
    { BeamRS.id = getId id,
      BeamRS.baseDistance = baseDistance,
      BeamRS.baseDuration = baseDuration
    }
