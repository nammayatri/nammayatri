{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Driver.GoHomeFeature.DriverHomeLocation where

import Domain.Types.Driver.GoHomeFeature.DriverHomeLocation
import qualified Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as Domain
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Common (getCurrentTime)
import Kernel.Types.Id as ID
import Lib.Utils ()
import qualified Sequelize as Se
import Storage.Beam.Driver.GoHomeFeature.DriverHomeLocation as BeamDHL

create :: MonadFlow m => Domain.DriverHomeLocation -> m ()
create = createWithKV

findById :: MonadFlow m => Id Domain.DriverHomeLocation -> m (Maybe Domain.DriverHomeLocation)
findById (ID.Id driverHomeLocId) = findOneWithKV [Se.Is BeamDHL.id $ Se.Eq driverHomeLocId]

findAllByDriverId :: MonadFlow m => Id Driver -> m [Domain.DriverHomeLocation]
findAllByDriverId (ID.Id driverId) = findAllWithKV [Se.Is BeamDHL.driverId $ Se.Eq driverId]

deleteById :: MonadFlow m => Id Domain.DriverHomeLocation -> m ()
deleteById (ID.Id driverHomeLocId) = deleteWithKV [Se.Is BeamDHL.id $ Se.Eq driverHomeLocId]

deleteByDriverId :: MonadFlow m => Id Driver -> m ()
deleteByDriverId (ID.Id driverId) = deleteWithKV [Se.Is BeamDHL.driverId $ Se.Eq driverId]

updateHomeLocationById :: MonadFlow m => Id Domain.DriverHomeLocation -> Domain.UpdateDriverHomeLocation -> m ()
updateHomeLocationById homeLocationId driverHomeLocation = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDHL.lat driverHomeLocation.lat, Se.Set BeamDHL.lon driverHomeLocation.lon, Se.Set BeamDHL.address driverHomeLocation.address, Se.Set BeamDHL.tag driverHomeLocation.tag, Se.Set BeamDHL.updatedAt now]
    [Se.Is BeamDHL.id $ Se.Eq $ getId homeLocationId]

instance FromTType' BeamDHL.DriverHomeLocation Domain.DriverHomeLocation where
  fromTType' BeamDHL.DriverHomeLocationT {..} = do
    pure $
      Just
        Domain.DriverHomeLocation
          { id = ID.Id id,
            driverId = ID.Id driverId,
            ..
          }

instance ToTType' BeamDHL.DriverHomeLocation Domain.DriverHomeLocation where
  toTType' Domain.DriverHomeLocation {..} =
    BeamDHL.DriverHomeLocationT
      { id = id.getId,
        driverId = getId driverId,
        ..
      }
