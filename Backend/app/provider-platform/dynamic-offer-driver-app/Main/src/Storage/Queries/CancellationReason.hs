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
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude hiding (isNothing)
import Kernel.Storage.Esqueleto as Esq
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as BeamCR
import Storage.Tabular.CancellationReason

findAll :: Transactionable m => m [CancellationReason]
findAll = Esq.findAll $ do
  cancellationReason <- from $ table @CancellationReasonT
  where_ $ cancellationReason ^. CancellationReasonEnabled
  orderBy [desc $ cancellationReason ^. CancellationReasonPriority]
  return cancellationReason

findAll' :: L.MonadFlow m => m [CancellationReason]
findAll' = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    -- Just dbCOnf' -> either (pure Nothing) (transformBeamCallStatusToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamCT.id $ Se.Eq callStatusId]
    -- findAllWithKVConnector
    Just dbCOnf' -> either (pure []) (transformBeamCancReasonToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamCR.enabled $ Se.Eq True]
    Nothing -> pure []

transformBeamCancReasonToDomain :: BeamCR.CancellationReason -> CancellationReason
transformBeamCancReasonToDomain BeamCR.CancellationReasonT {..} = do
  CancellationReason
    { reasonCode = CancellationReasonCode $ reasonCode,
      description = description,
      enabled = enabled,
      priority = priority
    }

transformBeamCancellationReasonToDomain :: BeamCR.CancellationReason -> CancellationReason
transformBeamCancellationReasonToDomain BeamCR.CancellationReasonT {..} = do
  CancellationReason
    { reasonCode = CancellationReasonCode $ reasonCode,
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
