{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RideDetails where

import qualified Domain.Types.Ride as SR
import Domain.Types.RideDetails as DRD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RideDetails as BeamRD

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DRD.RideDetails -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SR.Ride -> m (Maybe RideDetails)
findById (Id rideDetailsId) = findOneWithKV [Se.Is BeamRD.id $ Se.Eq rideDetailsId]

instance FromTType' BeamRD.RideDetails RideDetails where
  fromTType' BeamRD.RideDetailsT {..} = do
    pure $
      Just
        RideDetails
          { id = Id id,
            driverName = driverName,
            driverNumber = EncryptedHashed <$> (Encrypted <$> driverNumberEncrypted) <*> driverNumberHash,
            driverCountryCode = driverCountryCode,
            vehicleNumber = vehicleNumber,
            vehicleColor = vehicleColor,
            vehicleVariant = vehicleVariant,
            vehicleModel = vehicleModel,
            vehicleClass = vehicleClass,
            fleetOwnerId = fleetOwnerId
          }

instance ToTType' BeamRD.RideDetails RideDetails where
  toTType' RideDetails {..} = do
    BeamRD.RideDetailsT
      { BeamRD.id = getId id,
        BeamRD.driverName = driverName,
        BeamRD.driverNumberEncrypted = driverNumber <&> unEncrypted . (.encrypted),
        BeamRD.driverNumberHash = driverNumber <&> (.hash),
        BeamRD.driverCountryCode = driverCountryCode,
        BeamRD.vehicleNumber = vehicleNumber,
        BeamRD.vehicleColor = vehicleColor,
        BeamRD.vehicleVariant = vehicleVariant,
        BeamRD.vehicleModel = vehicleModel,
        BeamRD.vehicleClass = vehicleClass,
        BeamRD.fleetOwnerId = fleetOwnerId
      }
