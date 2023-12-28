{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person.PersonDisability where

import Domain.Types.Person
import qualified Domain.Types.Person.PersonDisability as Domain
import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, deleteWithKV, findOneWithKV, updateWithKV)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Person.PersonDisability as BeamPD hiding (Id)

create :: MonadFlow m => Domain.PersonDisability -> m ()
create = createWithKV

findByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Domain.PersonDisability)
findByPersonId (Id personId) = findOneWithKV [Se.Is BeamPD.personId $ Se.Eq personId]

updateDisabilityByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Text -> Text -> Maybe Text -> m ()
updateDisabilityByPersonId (Id personId) disabilityId tag description = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamPD.disabilityId disabilityId,
      Se.Set BeamPD.tag tag,
      Se.Set BeamPD.description description,
      Se.Set BeamPD.updatedAt now
    ]
    [Se.Is BeamPD.personId (Se.Eq personId)]

deleteByPersonId :: MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamPD.personId (Se.Eq personId)]

instance FromTType' BeamPD.PersonDisability Domain.PersonDisability where
  fromTType' BeamPD.PersonDisabilityT {..} = do
    pure $
      Just $
        Domain.PersonDisability
          { personId = Id personId,
            ..
          }

instance ToTType' BeamPD.PersonDisability Domain.PersonDisability where
  toTType' Domain.PersonDisability {..} = do
    BeamPD.PersonDisabilityT
      { BeamPD.personId = getId personId,
        ..
      }
