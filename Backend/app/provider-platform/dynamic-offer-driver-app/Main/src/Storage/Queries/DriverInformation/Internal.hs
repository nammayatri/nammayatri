{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.Queries.DriverInformation.Internal where

import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Person as DP
import Domain.Types.VehicleCategory as DV
import Kernel.Beam.Functions (findAllWithKV, findOneWithKV, updateOneWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Queries.OrphanInstances.DriverInformation ()

updateOnboardingVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe DV.VehicleCategory -> Id DP.Person -> m ())
updateOnboardingVehicleCategory onboardingVehicleCategory driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDI.onboardingVehicleCategory onboardingVehicleCategory, Se.Set BeamDI.updatedAt _now] [Se.Is BeamDI.driverId $ Se.Eq driverId.getId]

getDriverInfos ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Text] ->
  m [DriverInfo.DriverInformation]
getDriverInfos personKeys = do
  findAllWithKV [Se.Is BeamDI.driverId $ Se.In personKeys]

getSpecialLocWarriorDriverInfo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe DriverInfo.DriverInformation)
getSpecialLocWarriorDriverInfo driverId =
  findOneWithKV
    [ Se.And
        ( [ Se.Is BeamDI.driverId $ Se.Eq driverId,
            Se.Is BeamDI.blocked $ Se.Eq False
          ]
            <> [Se.Is BeamDI.isSpecialLocWarrior $ Se.Eq (Just True)]
        )
    ]
