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
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, updateWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.MetaData as BeamMD

-- create :: MetaData -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => MetaData -> m ()
create = createWithKV

-- updateMetaData :: Id Person -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> SqlDB ()
-- updateMetaData personId device deviceOS deviceDateTime appPermissions = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ MetaDataDevice =. val device,
--         MetaDataDeviceOS =. val deviceOS,
--         MetaDataDeviceDateTime =. val deviceDateTime,
--         MetaDataAppPermissions =. val appPermissions,
--         MetaDataUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. MetaDataDriverId ==. val

updateMetaData :: (L.MonadFlow m, Log m, MonadTime m) => Id Person -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> m ()
updateMetaData personId device deviceOS deviceDateTime appPermissions = do
  now <- getCurrentTime
  updateWithKV
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
