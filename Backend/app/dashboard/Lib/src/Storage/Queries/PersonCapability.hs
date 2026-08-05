{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PersonCapability where

import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.PersonCapability as BeamPC

create :: BeamFlow m r => DC.PersonCapability -> m ()
create = createWithKV

findAllByPersonId :: BeamFlow m r => Id DP.Person -> m [DC.PersonCapability]
findAllByPersonId personId = findAllWithKV [Se.Is BeamPC.personId $ Se.Eq $ getId personId]

deleteByPersonIdAndCapabilityId :: BeamFlow m r => Id DP.Person -> Id DC.Capability -> m ()
deleteByPersonIdAndCapabilityId personId capabilityId =
  deleteWithKV
    [ Se.And
        [ Se.Is BeamPC.personId $ Se.Eq $ getId personId,
          Se.Is BeamPC.capabilityId $ Se.Eq $ getId capabilityId
        ]
    ]

instance FromTType' BeamPC.PersonCapability DC.PersonCapability where
  fromTType' BeamPC.PersonCapabilityT {..} = do
    return $
      Just
        DC.PersonCapability
          { personId = Id personId,
            capabilityId = Id capabilityId,
            grantedBy = Id <$> grantedBy,
            ..
          }

instance ToTType' BeamPC.PersonCapability DC.PersonCapability where
  toTType' DC.PersonCapability {..} =
    BeamPC.PersonCapabilityT
      { personId = getId personId,
        capabilityId = getId capabilityId,
        grantedBy = getId <$> grantedBy,
        ..
      }
