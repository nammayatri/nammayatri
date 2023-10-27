{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person.Internal where

import qualified Domain.Types.Person as DP
import Domain.Types.Vehicle as DV
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.Instances.Person ()

getDrivers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [DV.Vehicle] ->
  m [DP.Person]
getDrivers vehicles = findAllWithKV [Se.And [Se.Is BeamP.id $ Se.In personKeys, Se.Is BeamP.role $ Se.Eq DP.DRIVER]]
  where
    personKeys = getId <$> fetchDriverIDsFromVehicle vehicles

fetchDriverIDsFromVehicle :: [Vehicle] -> [Id DP.Person]
fetchDriverIDsFromVehicle = map (.driverId)
