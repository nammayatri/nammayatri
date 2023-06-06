{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.CancellationReason where

import Domain.Types.CancellationReason
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude hiding (isNothing)
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as BeamCR

findAll :: L.MonadFlow m => m [CancellationReason]
findAll = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamCancellationReasonToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamCR.enabled $ Se.Eq True] (Se.Desc BeamCR.priority) Nothing Nothing
    Nothing -> pure []

transformBeamCancellationReasonToDomain :: BeamCR.CancellationReason -> CancellationReason
transformBeamCancellationReasonToDomain BeamCR.CancellationReasonT {..} = do
  CancellationReason
    { reasonCode = CancellationReasonCode reasonCode,
      description = description,
      enabled = enabled,
      priority = priority
    }

transformDomainCancellationReasonToBeam :: CancellationReason -> BeamCR.CancellationReason
transformDomainCancellationReasonToBeam CancellationReason {..} =
  BeamCR.CancellationReasonT
    { BeamCR.reasonCode = (\(CancellationReasonCode x) -> x) reasonCode,
      BeamCR.description = description,
      BeamCR.enabled = enabled,
      BeamCR.priority = priority
    }
