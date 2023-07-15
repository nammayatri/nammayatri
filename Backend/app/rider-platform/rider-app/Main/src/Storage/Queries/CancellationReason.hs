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
import qualified Domain.Types.CancellationReason as Domain
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as BeamCR

-- findAll :: Transactionable m => CancellationStage -> m [CancellationReason]
-- findAll cancStage =
--   Esq.findAll $ do
--     cancellationReason <- from $ table @CancellationReasonT
--     where_ $
--       cancellationReason ^. CancellationReasonEnabled
--         &&. case cancStage of
--           OnSearch -> cancellationReason ^. CancellationReasonOnSearch
--           OnConfirm -> cancellationReason ^. CancellationReasonOnConfirm
--           OnAssign -> cancellationReason ^. CancellationReasonOnAssign
--     orderBy [desc $ cancellationReason ^. CancellationReasonPriority]
--     return cancellationReason

findAll :: L.MonadFlow m => CancellationStage -> m [CancellationReason]
findAll cancStage = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  seCaseCondition <- case cancStage of
    OnSearch -> pure $ Se.Is BeamCR.onSearch $ Se.Eq True
    OnConfirm -> pure $ Se.Is BeamCR.onConfirm $ Se.Eq True
    OnAssign -> pure $ Se.Is BeamCR.onAssign $ Se.Eq True
  let modelName = Se.modelTableName @BeamCR.CancellationReasonT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamCancellationReasonToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamCR.enabled $ Se.Eq True, seCaseCondition]] (Se.Desc BeamCR.priority) Nothing Nothing
    Nothing -> pure []

transformBeamCancellationReasonToDomain :: BeamCR.CancellationReason -> CancellationReason
transformBeamCancellationReasonToDomain BeamCR.CancellationReasonT {..} = do
  CancellationReason
    { reasonCode = Domain.CancellationReasonCode reasonCode,
      description = description,
      enabled = enabled,
      onSearch = onSearch,
      onConfirm = onConfirm,
      onAssign = onAssign,
      priority = priority
    }

transformDomainCancellationReasonToBeam :: CancellationReason -> BeamCR.CancellationReason
transformDomainCancellationReasonToBeam CancellationReason {..} =
  BeamCR.CancellationReasonT
    { BeamCR.reasonCode = let (Domain.CancellationReasonCode rc) = reasonCode in rc,
      BeamCR.description = description,
      BeamCR.enabled = enabled,
      BeamCR.onSearch = onSearch,
      BeamCR.onConfirm = onConfirm,
      BeamCR.onAssign = onAssign,
      BeamCR.priority = priority
    }
