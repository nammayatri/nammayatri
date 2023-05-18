{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RideDetails where

import qualified Domain.Types.Ride as SR
import Domain.Types.RideDetails
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption (DbHash (..), Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.RideDetails as BeamRD
import Storage.Tabular.RideDetails ()

create :: RideDetails -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id SR.Ride ->
  m (Maybe RideDetails)
findById = Esq.findById

transformBeamRideDetailsToDomain :: BeamRD.RideDetails -> RideDetails
transformBeamRideDetailsToDomain BeamRD.RideDetailsT {..} = do
  RideDetails
    { id = Id id,
      driverName = driverName,
      driverNumber = EncryptedHashed <$> (Encrypted <$> driverNumberEncrypted) <*> driverNumberHash,
      driverCountryCode = driverCountryCode,
      vehicleNumber = vehicleNumber,
      vehicleColor = vehicleColor,
      vehicleVariant = vehicleVariant,
      vehicleModel = vehicleModel,
      vehicleClass = vehicleClass
    }

transformDomainRideDetailsToBeam :: RideDetails -> BeamRD.RideDetails
transformDomainRideDetailsToBeam RideDetails {..} =
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
      BeamRD.vehicleClass = vehicleClass
    }
