{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.CancellationReason where

import Domain.Types.CancellationReason
import qualified Domain.Types.CancellationReason as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as BeamCR

-- Not querying by Id. In case the table is enabled someday, better to route this through DB
findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => CancellationStage -> m [CancellationReason]
findAll cancStage = do
  seCaseCondition <- case cancStage of
    OnSearch -> pure $ Se.Is BeamCR.onSearch $ Se.Eq True
    OnConfirm -> pure $ Se.Is BeamCR.onConfirm $ Se.Eq True
    OnAssign -> pure $ Se.Is BeamCR.onAssign $ Se.Eq True
  findAllWithOptionsDb [Se.And [Se.Is BeamCR.enabled $ Se.Eq True, seCaseCondition]] (Se.Desc BeamCR.priority) Nothing Nothing

instance FromTType' BeamCR.CancellationReason CancellationReason where
  fromTType' BeamCR.CancellationReasonT {..} = do
    pure $
      Just
        CancellationReason
          { reasonCode = Domain.CancellationReasonCode reasonCode,
            description = description,
            enabled = enabled,
            onSearch = onSearch,
            onConfirm = onConfirm,
            onAssign = onAssign,
            priority = priority
          }

instance ToTType' BeamCR.CancellationReason CancellationReason where
  toTType' CancellationReason {..} = do
    BeamCR.CancellationReasonT
      { BeamCR.reasonCode = let (Domain.CancellationReasonCode rc) = reasonCode in rc,
        BeamCR.description = description,
        BeamCR.enabled = enabled,
        BeamCR.onSearch = onSearch,
        BeamCR.onConfirm = onConfirm,
        BeamCR.onAssign = onAssign,
        BeamCR.priority = priority
      }
