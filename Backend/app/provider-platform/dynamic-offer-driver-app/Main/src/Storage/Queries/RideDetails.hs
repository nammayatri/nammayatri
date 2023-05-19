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
import Domain.Types.RideDetails as DRD
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.RideDetails as BeamRD
import Storage.Tabular.RideDetails ()
import qualified Storage.Tabular.VechileNew as VN

create :: RideDetails -> SqlDB ()
create = Esq.create

-- create' :: L.MonadFlow m => DRD.RideDetails -> m (MeshResult ())
-- create' rideDetails = do
--   dbConf <- L.getOption Extra.EulerPsqlDbCfg
--   case dbConf of
--     Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainRideDetailsToBeam rideDetails)
--     Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById ::
  Transactionable m =>
  Id SR.Ride ->
  m (Maybe RideDetails)
findById = Esq.findById

-- findById' :: L.MonadFlow m => Id SR.Ride -> m (Maybe RideDetails)
-- findById' (Id rideDetailsId) = do
--   dbConf <- L.getOption Extra.EulerPsqlDbCfg
--   case dbConf of
--     Just dbCOnf' -> either (pure Nothing) (transformBeamRideDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamRD.id $ Se.Eq rideDetailsId]
--     Nothing -> pure Nothing

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
