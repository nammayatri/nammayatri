{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RoleCapability where

import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.RoleCapability as BeamRC

create :: BeamFlow m r => DC.RoleCapability -> m ()
create = createWithKV

findAllByRoleId :: BeamFlow m r => Id DRole.Role -> m [DC.RoleCapability]
findAllByRoleId roleId = findAllWithKV [Se.Is BeamRC.roleId $ Se.Eq $ getId roleId]

deleteByRoleIdAndCapabilityId :: BeamFlow m r => Id DRole.Role -> Id DC.Capability -> m ()
deleteByRoleIdAndCapabilityId roleId capabilityId =
  deleteWithKV
    [ Se.And
        [ Se.Is BeamRC.roleId $ Se.Eq $ getId roleId,
          Se.Is BeamRC.capabilityId $ Se.Eq $ getId capabilityId
        ]
    ]

instance FromTType' BeamRC.RoleCapability DC.RoleCapability where
  fromTType' BeamRC.RoleCapabilityT {..} = do
    return $
      Just
        DC.RoleCapability
          { roleId = Id roleId,
            capabilityId = Id capabilityId,
            ..
          }

instance ToTType' BeamRC.RoleCapability DC.RoleCapability where
  toTType' DC.RoleCapability {..} =
    BeamRC.RoleCapabilityT
      { roleId = getId roleId,
        capabilityId = getId capabilityId,
        ..
      }
