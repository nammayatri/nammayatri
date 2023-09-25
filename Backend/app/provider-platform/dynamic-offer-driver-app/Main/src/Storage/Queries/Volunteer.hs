{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Volunteer where

import Domain.Types.Volunteer
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.Volunteer as BeamV

create :: MonadFlow m => Volunteer -> m ()
create = createWithKV

findById :: (MonadFlow m) => Id Volunteer -> m (Maybe Volunteer)
findById (Id volunteerId) = findOneWithKV [Se.Is BeamV.id $ Se.Eq volunteerId]

findAllByPlace :: (MonadFlow m) => Text -> m [Volunteer]
findAllByPlace place = findAllWithKV [Se.Is BeamV.place $ Se.Eq place]

instance FromTType' BeamV.Volunteer Volunteer where
  fromTType' BeamV.VolunteerT {..} = do
    pure $
      Just
        Volunteer
          { id = Id id,
            place = place,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamV.Volunteer Volunteer where
  toTType' Volunteer {..} = do
    BeamV.VolunteerT
      { BeamV.id = getId id,
        BeamV.place = place,
        BeamV.createdAt = createdAt,
        BeamV.updatedAt = updatedAt
      }
