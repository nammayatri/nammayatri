{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal where

import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as Domain
import Domain.Types.Person (Driver)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto (Point (..))
import Kernel.Types.Id
import Kernel.Utils.Common hiding (Value)
import qualified Sequelize as Se
import qualified Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest as BeamDDGR

getDriverGoHomeReqNearby :: (MonadFlow m) => [Id Driver] -> m [DriverGoHomeRequest]
getDriverGoHomeReqNearby driverIds = do
  findAllWithKV [Se.And [Se.Is BeamDDGR.driverId $ Se.In $ getId <$> driverIds, Se.Is BeamDDGR.status $ Se.Eq DDGR.ACTIVE]]

instance FromTType' BeamDDGR.DriverGoHomeRequest Domain.DriverGoHomeRequest where
  fromTType' BeamDDGR.DriverGoHomeRequestT {..} = do
    pure $
      Just
        Domain.DriverGoHomeRequest
          { id = Id id,
            driverId = Id driverId,
            mbReachedHome = reachedHome,
            ..
          }

instance ToTType' BeamDDGR.DriverGoHomeRequest Domain.DriverGoHomeRequest where
  toTType' Domain.DriverGoHomeRequest {..} = do
    BeamDDGR.DriverGoHomeRequestT
      { id = getId id,
        driverId = getId driverId,
        point = Point,
        reachedHome = mbReachedHome,
        ..
      }
