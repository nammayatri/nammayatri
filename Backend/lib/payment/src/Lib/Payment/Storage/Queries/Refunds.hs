{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.Refunds where

import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findOneWithKV, updateOneWithKV)
import Kernel.External.Payment.Interface (RefundsData)
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Payment.Domain.Types.Refunds as Domain
import Lib.Payment.Storage.Beam.BeamFlow
import Lib.Payment.Storage.Beam.Refunds as BeamI hiding (Id)
import qualified Sequelize as Se

create :: BeamFlow m r => Domain.Refunds -> m ()
create = createWithKV

findById :: BeamFlow m r => Id Domain.Refunds -> m (Maybe Domain.Refunds)
findById (Id refundId) = findOneWithKV [Se.Is BeamI.id $ Se.Eq refundId]

findByShortId :: BeamFlow m r => Text -> m (Maybe Domain.Refunds)
findByShortId shortId = findOneWithKV [Se.Is BeamI.shortId $ Se.Eq shortId]

updateRefundsEntryByResponse :: BeamFlow m r => RefundsData -> Id Domain.Refunds -> m ()
updateRefundsEntryByResponse response refundId = do
  updateOneWithKV
    [ Se.Set BeamI.initiatedBy response.initiatedBy,
      Se.Set BeamI.idAssignedByServiceProvider (Just response.idAssignedByServiceProvider),
      Se.Set BeamI.errorMessage response.errorMessage,
      Se.Set BeamI.errorCode response.errorCode,
      Se.Set BeamI.status response.status
    ]
    [ Se.Is BeamI.id $ Se.Eq refundId.getId
    ]

instance FromTType' BeamI.Refunds Domain.Refunds where
  fromTType' BeamI.RefundsT {..} = do
    pure $
      Just
        Domain.Refunds
          { id = Id id,
            shortId,
            orderId = Id orderId,
            refundAmount,
            idAssignedByServiceProvider,
            merchantId,
            status,
            errorMessage,
            errorCode,
            initiatedBy,
            ..
          }

instance ToTType' BeamI.Refunds Domain.Refunds where
  toTType' Domain.Refunds {..} =
    BeamI.RefundsT
      { id = id.getId,
        shortId,
        orderId = orderId.getId,
        refundAmount,
        idAssignedByServiceProvider,
        merchantId,
        status,
        errorMessage,
        errorCode,
        initiatedBy,
        ..
      }
