{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PersonRoleAssignment where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonRoleAssignment as DPRA
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.PersonRoleAssignment as BeamPRA

create :: BeamFlow m r => DPRA.PersonRoleAssignment -> m ()
create = createWithKV

-- | Every assignment a person has, whatever its status.
findAllByPersonId :: BeamFlow m r => Id DP.Person -> m [DPRA.PersonRoleAssignment]
findAllByPersonId personId =
  findAllWithKV [Se.Is BeamPRA.personId $ Se.Eq $ getId personId]

-- | What a person may act on right now in a merchant: only ACTIVE assignments grant access.
findAllActiveByPersonIdAndMerchantId :: BeamFlow m r => Id DP.Person -> Id DMerchant.Merchant -> m [DPRA.PersonRoleAssignment]
findAllActiveByPersonIdAndMerchantId personId merchantId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamPRA.personId $ Se.Eq $ getId personId,
          Se.Is BeamPRA.merchantId $ Se.Eq $ getId merchantId,
          Se.Is BeamPRA.status $ Se.Eq DPRA.ACTIVE
        ]
    ]

-- | The assignment a person already holds for a role on a scope, if any.
findByPersonRoleScope ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DRole.Role ->
  DPRA.ScopeType ->
  Text ->
  Id DMerchant.Merchant ->
  m (Maybe DPRA.PersonRoleAssignment)
findByPersonRoleScope personId roleId scopeType scopeId merchantId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamPRA.personId $ Se.Eq $ getId personId,
          Se.Is BeamPRA.roleId $ Se.Eq $ getId roleId,
          Se.Is BeamPRA.scopeType $ Se.Eq scopeType,
          Se.Is BeamPRA.scopeId $ Se.Eq scopeId,
          Se.Is BeamPRA.merchantId $ Se.Eq $ getId merchantId
        ]
    ]

-- | Everyone's active assignments of one role.
findAllActiveByRoleId :: BeamFlow m r => Id DRole.Role -> m [DPRA.PersonRoleAssignment]
findAllActiveByRoleId roleId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamPRA.roleId $ Se.Eq $ getId roleId,
          Se.Is BeamPRA.status $ Se.Eq DPRA.ACTIVE
        ]
    ]

findById :: BeamFlow m r => Id DPRA.PersonRoleAssignment -> m (Maybe DPRA.PersonRoleAssignment)
findById assignmentId = findOneWithKV [Se.Is BeamPRA.id $ Se.Eq $ getId assignmentId]

-- | Everyone who holds, or was offered, a role on a scope, whatever the status. For a fleet this is its admins.
findAllByScope :: BeamFlow m r => DPRA.ScopeType -> Text -> Id DMerchant.Merchant -> m [DPRA.PersonRoleAssignment]
findAllByScope scopeType scopeId merchantId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamPRA.scopeType $ Se.Eq scopeType,
          Se.Is BeamPRA.scopeId $ Se.Eq scopeId,
          Se.Is BeamPRA.merchantId $ Se.Eq $ getId merchantId
        ]
    ]

-- | A person's assignments of one scope kind, whatever their status. The sync compares these with
-- what the person's role and city access say they should hold.
findAllByPersonIdAndScopeType :: BeamFlow m r => Id DP.Person -> DPRA.ScopeType -> m [DPRA.PersonRoleAssignment]
findAllByPersonIdAndScopeType personId scopeType =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamPRA.personId $ Se.Eq $ getId personId,
          Se.Is BeamPRA.scopeType $ Se.Eq scopeType
        ]
    ]

updateStatus :: BeamFlow m r => DPRA.AssignmentStatus -> Id DPRA.PersonRoleAssignment -> m ()
updateStatus status assignmentId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamPRA.status status, Se.Set BeamPRA.updatedAt now]
    [Se.Is BeamPRA.id $ Se.Eq $ getId assignmentId]

updateStatusByIds :: BeamFlow m r => DPRA.AssignmentStatus -> [Id DPRA.PersonRoleAssignment] -> m ()
updateStatusByIds _ [] = pure ()
updateStatusByIds status assignmentIds = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamPRA.status status, Se.Set BeamPRA.updatedAt now]
    [Se.Is BeamPRA.id $ Se.In $ getId <$> assignmentIds]

-- | Make a row active as a role an admin granted by hand.
grantManually :: BeamFlow m r => Id DP.Person -> Id DPRA.PersonRoleAssignment -> m ()
grantManually grantedBy assignmentId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamPRA.status DPRA.ACTIVE,
      Se.Set BeamPRA.manuallyGranted True,
      Se.Set BeamPRA.grantedBy (Just $ getId grantedBy),
      Se.Set BeamPRA.updatedAt now
    ]
    [Se.Is BeamPRA.id $ Se.Eq $ getId assignmentId]

-- | Offer a revoked row again, only while it is still revoked. A person has one row per role and scope, so a later
-- offer has to reuse it. It is a new offer, so it takes the new inviter, handover and time.
offerAgain :: BeamFlow m r => DPRA.PersonRoleAssignment -> m ()
offerAgain offer = do
  updateOneWithKV
    [ Se.Set BeamPRA.status offer.status,
      Se.Set BeamPRA.grantedBy (getId <$> offer.grantedBy),
      Se.Set BeamPRA.handoverFrom (getId <$> offer.handoverFrom),
      Se.Set BeamPRA.manuallyGranted offer.manuallyGranted,
      Se.Set BeamPRA.createdAt offer.createdAt,
      Se.Set BeamPRA.updatedAt offer.updatedAt
    ]
    [ Se.And
        [ Se.Is BeamPRA.id $ Se.Eq $ getId offer.id,
          Se.Is BeamPRA.status $ Se.Eq DPRA.REVOKED
        ]
    ]

-- | Bring rows back for the sync. Whatever they were before, they are the sync's to manage from here on.
reactivateAsSynced :: BeamFlow m r => [Id DPRA.PersonRoleAssignment] -> m ()
reactivateAsSynced [] = pure ()
reactivateAsSynced assignmentIds = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamPRA.status DPRA.ACTIVE, Se.Set BeamPRA.manuallyGranted False, Se.Set BeamPRA.updatedAt now]
    [Se.Is BeamPRA.id $ Se.In $ getId <$> assignmentIds]

-- | Make an offer active, only while it is still waiting for an answer, so that one taken back at the same
-- moment stays taken back.
activateIfInvited :: BeamFlow m r => Id DPRA.PersonRoleAssignment -> m ()
activateIfInvited assignmentId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamPRA.status DPRA.ACTIVE, Se.Set BeamPRA.updatedAt now]
    [ Se.And
        [ Se.Is BeamPRA.id $ Se.Eq $ getId assignmentId,
          Se.Is BeamPRA.status $ Se.Eq DPRA.INVITED
        ]
    ]

-- | Remove a departing person's assignments before the person row is deleted
-- (person_role_assignment.person_id is a NOT NULL FK to person).
deleteAllByPersonId :: BeamFlow m r => Id DP.Person -> m ()
deleteAllByPersonId personId =
  deleteWithKV [Se.Is BeamPRA.personId $ Se.Eq $ getId personId]

instance FromTType' BeamPRA.PersonRoleAssignment DPRA.PersonRoleAssignment where
  fromTType' BeamPRA.PersonRoleAssignmentT {..} =
    return $
      Just
        DPRA.PersonRoleAssignment
          { id = Id id,
            personId = Id personId,
            roleId = Id roleId,
            merchantId = Id merchantId,
            grantedBy = Id <$> grantedBy,
            handoverFrom = Id <$> handoverFrom,
            ..
          }

instance ToTType' BeamPRA.PersonRoleAssignment DPRA.PersonRoleAssignment where
  toTType' DPRA.PersonRoleAssignment {..} =
    BeamPRA.PersonRoleAssignmentT
      { id = getId id,
        personId = getId personId,
        roleId = getId roleId,
        merchantId = getId merchantId,
        grantedBy = getId <$> grantedBy,
        handoverFrom = getId <$> handoverFrom,
        ..
      }
