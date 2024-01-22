{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person.PersonDefaultEmergencyNumber where

import Domain.Types.Person
import Domain.Types.Person.PersonDefaultEmergencyNumber
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Person.PersonDefaultEmergencyNumber as BeamPDEN

create :: MonadFlow m => PersonDefaultEmergencyNumber -> m ()
create = createWithKV

createMany :: MonadFlow m => [PersonDefaultEmergencyNumber] -> m ()
createMany = traverse_ create

replaceAll :: MonadFlow m => Id Person -> [PersonDefaultEmergencyNumber] -> m ()
replaceAll (Id personId) pdenList = do
  deleteWithKV [Se.Is BeamPDEN.personId $ Se.Eq personId]
  createMany pdenList

findAllByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [PersonDefaultEmergencyNumber]
findAllByPersonId (Id personId) = findAllWithKV [Se.Is BeamPDEN.personId $ Se.Eq personId]

findAllByContactPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [PersonDefaultEmergencyNumber]
findAllByContactPersonId (Id contactPersonId) = findAllWithKV [Se.Is BeamPDEN.contactPersonId $ Se.Eq (Just contactPersonId)]

updateEmergencyContactPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DbHash -> Id Person -> m ()
updateEmergencyContactPersonId dbHash (Id personId) = do
  updateWithKV
    [ Se.Set BeamPDEN.contactPersonId (Just personId)
    ]
    [ Se.Is BeamPDEN.mobileNumberHash $ Se.Eq dbHash
    ]

instance FromTType' BeamPDEN.PersonDefaultEmergencyNumber PersonDefaultEmergencyNumber where
  fromTType' BeamPDEN.PersonDefaultEmergencyNumberT {..} = do
    pure $
      Just
        PersonDefaultEmergencyNumber
          { personId = Id personId,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            contactPersonId = Id <$> contactPersonId,
            priority = priority,
            ..
          }

instance ToTType' BeamPDEN.PersonDefaultEmergencyNumber PersonDefaultEmergencyNumber where
  toTType' PersonDefaultEmergencyNumber {..} = do
    BeamPDEN.PersonDefaultEmergencyNumberT
      { BeamPDEN.personId = getId personId,
        BeamPDEN.name = name,
        BeamPDEN.mobileCountryCode = mobileCountryCode,
        BeamPDEN.mobileNumberHash = mobileNumber.hash,
        BeamPDEN.mobileNumberEncrypted = unEncrypted (mobileNumber.encrypted),
        BeamPDEN.contactPersonId = getId <$> contactPersonId,
        BeamPDEN.enableForFollowing = enableForFollowing,
        BeamPDEN.priority = priority,
        BeamPDEN.createdAt = createdAt
      }
