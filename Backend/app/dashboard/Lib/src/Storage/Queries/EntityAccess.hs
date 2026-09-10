{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.EntityAccess where

import Data.List (sortOn)
import qualified Domain.Types.Entity as DEntity
import qualified Domain.Types.EntityAccess as DEA
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.EntityAccess as BeamEA
import Storage.Queries.Entity ()

-- Grants from one bulkUpsert share a single createdAt, so id breaks the tie and keeps the primary depot stable.
grantOrder :: DEA.EntityAccess -> (UTCTime, Text)
grantOrder g = (g.createdAt, g.id.getId)

create :: BeamFlow m r => DEA.EntityAccess -> m ()
create = createWithKV

-- Merchant-scoped: a person may hold depots under several merchants, and an admin of one must
-- never see another merchant's grants on that shared person.
findAllByPersonIdsAndMerchantId :: BeamFlow m r => [Id DP.Person] -> Id DMerchant.Merchant -> m [DEA.EntityAccess]
findAllByPersonIdsAndMerchantId [] _ = pure []
findAllByPersonIdsAndMerchantId personIds merchantId =
  sortOn grantOrder
    <$> findAllWithKV
      [ Se.And
          [ Se.Is BeamEA.personId $ Se.In $ getId <$> personIds,
            Se.Is BeamEA.merchantId $ Se.Eq $ getId merchantId
          ]
      ]

-- Scoped to one merchant so a revoke can never reach a grant the caller does not own, even if
-- the same person holds entities under another merchant.
deleteManyByPersonIdAndEntityIds :: BeamFlow m r => Id DMerchant.Merchant -> [(Id DP.Person, [Id DEntity.Entity])] -> m ()
deleteManyByPersonIdAndEntityIds merchantId revocations = do
  let clauses =
        [ Se.And
            [ Se.Is BeamEA.personId $ Se.Eq $ getId personId,
              Se.Is BeamEA.entityId $ Se.In $ getId <$> entityIds
            ]
          | (personId, entityIds) <- revocations,
            not (null entityIds)
        ]
  unless (null clauses) $
    deleteWithKV [Se.And [Se.Is BeamEA.merchantId $ Se.Eq $ getId merchantId, Se.Or clauses]]

deleteAllByPersonId :: BeamFlow m r => Id DP.Person -> m ()
deleteAllByPersonId personId = deleteWithKV [Se.Is BeamEA.personId $ Se.Eq $ getId personId]

instance FromTType' BeamEA.EntityAccess DEA.EntityAccess where
  fromTType' BeamEA.EntityAccessT {..} =
    pure $
      Just
        DEA.EntityAccess
          { id = Id id,
            personId = Id personId,
            entityId = Id entityId,
            merchantId = Id merchantId,
            ..
          }

instance ToTType' BeamEA.EntityAccess DEA.EntityAccess where
  toTType' DEA.EntityAccess {..} =
    BeamEA.EntityAccessT
      { id = getId id,
        personId = getId personId,
        entityId = getId entityId,
        merchantId = getId merchantId,
        ..
      }
