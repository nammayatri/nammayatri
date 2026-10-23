{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RegistrationToken where

import Domain.Types.Person
import Domain.Types.RegistrationToken
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.RegistrationToken as BeamRT

-- | One token per person: the token is identity-only, so it is addressed by
-- token string or by person id. The former per-(merchant,city) lookups and
-- deletes are gone -- scope lives in merchant_access, not on the token.
create :: BeamFlow m r => RegistrationToken -> m ()
create = createWithKV

findByToken :: BeamFlow m r => RegToken -> m (Maybe RegistrationToken)
findByToken token =
  findOneWithKV [Se.Is BeamRT.token $ Se.Eq token]

findAllByPersonId :: BeamFlow m r => Id Person -> m [RegistrationToken]
findAllByPersonId personId =
  findAllWithKV [Se.Is BeamRT.personId $ Se.Eq $ getId personId]

deleteAllByPersonId :: BeamFlow m r => Id Person -> m ()
deleteAllByPersonId personId = deleteWithKV [Se.Is BeamRT.personId $ Se.Eq $ getId personId]

updateEnabledStatusByPersonId :: BeamFlow m r => Id Person -> Bool -> m ()
updateEnabledStatusByPersonId personId enabled =
  updateWithKV
    [Se.Set BeamRT.enabled enabled]
    [Se.Is BeamRT.personId $ Se.Eq $ getId personId]

deleteById :: BeamFlow m r => Id RegistrationToken -> m ()
deleteById registrationTokenId = deleteWithKV [Se.Is BeamRT.id $ Se.Eq $ getId registrationTokenId]

instance FromTType' BeamRT.RegistrationToken RegistrationToken where
  fromTType' BeamRT.RegistrationTokenT {..} = do
    return $
      Just
        RegistrationToken
          { id = Id id,
            personId = Id personId,
            ..
          }

instance ToTType' BeamRT.RegistrationToken RegistrationToken where
  toTType' RegistrationToken {..} =
    BeamRT.RegistrationTokenT
      { id = getId id,
        personId = getId personId,
        ..
      }
