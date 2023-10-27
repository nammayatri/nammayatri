{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MetaData where

import Domain.Types.MetaData
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.MetaData as BeamMD

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => MetaData -> m ()
create = createWithKV

updateMetaData :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> m ()
updateMetaData personId device deviceOS deviceDateTime appPermissions = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamMD.device device,
      Se.Set BeamMD.deviceOS deviceOS,
      Se.Set BeamMD.deviceDateTime deviceDateTime,
      Se.Set BeamMD.appPermissions appPermissions,
      Se.Set BeamMD.updatedAt now
    ]
    [Se.Is BeamMD.driverId $ Se.Eq $ getId personId]

instance FromTType' BeamMD.MetaData MetaData where
  fromTType' BeamMD.MetaDataT {..} = do
    pure $
      Just
        MetaData
          { driverId = Id driverId,
            device = device,
            deviceOS = deviceOS,
            deviceDateTime = deviceDateTime,
            appPermissions = appPermissions,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamMD.MetaData MetaData where
  toTType' MetaData {..} = do
    BeamMD.MetaDataT
      { BeamMD.driverId = getId driverId,
        BeamMD.device = device,
        BeamMD.deviceOS = deviceOS,
        BeamMD.deviceDateTime = deviceDateTime,
        BeamMD.appPermissions = appPermissions,
        BeamMD.createdAt = createdAt,
        BeamMD.updatedAt = updatedAt
      }
