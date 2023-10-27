{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Vehicle.Internal where

import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Vehicle as DV
import Kernel.Beam.Functions (findAllWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Vehicle as BeamV
import Storage.Queries.Vehicle ()

getVehicles ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) =>
  [DriverInfo.DriverInformation] ->
  m [DV.Vehicle]
getVehicles driverInfo = findAllWithKV [Se.Is BeamV.driverId $ Se.In personKeys]
  where
    personKeys = getId . DriverInfo.driverId <$> driverInfo
