{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ClientPersonInfo where

import qualified Domain.Types.BecknConfig
import qualified Domain.Types.ClientPersonInfo
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ClientPersonInfo as Beam

create :: KvDbFlow m r => (Domain.Types.ClientPersonInfo.ClientPersonInfo -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.ClientPersonInfo.ClientPersonInfo] -> m ())
createMany = traverse_ create

findAllByPersonId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.ClientPersonInfo.ClientPersonInfo])
findAllByPersonId (Kernel.Types.Id.Id personId) = do findAllWithKV [Se.Is Beam.personId $ Se.Eq personId]

findByPersonIdAndVehicleCategory ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Domain.Types.BecknConfig.VehicleCategory -> m (Maybe Domain.Types.ClientPersonInfo.ClientPersonInfo))
findByPersonIdAndVehicleCategory (Kernel.Types.Id.Id personId) vehicleCategory = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq personId, Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory]]

updateHasTakenValidRideCount :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Domain.Types.BecknConfig.VehicleCategory -> m ())
updateHasTakenValidRideCount rideCount (Kernel.Types.Id.Id personId) vehicleCategory = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.rideCount rideCount, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.personId $ Se.Eq personId, Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.ClientPersonInfo.ClientPersonInfo -> m (Maybe Domain.Types.ClientPersonInfo.ClientPersonInfo))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.ClientPersonInfo.ClientPersonInfo -> m ())
updateByPrimaryKey (Domain.Types.ClientPersonInfo.ClientPersonInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientId (Kernel.Types.Id.getId <$> clientId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.rideCount rideCount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ClientPersonInfo Domain.Types.ClientPersonInfo.ClientPersonInfo where
  fromTType' (Beam.ClientPersonInfoT {..}) = do
    pure $
      Just
        Domain.Types.ClientPersonInfo.ClientPersonInfo
          { clientId = Kernel.Types.Id.Id <$> clientId,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            rideCount = rideCount,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.ClientPersonInfo Domain.Types.ClientPersonInfo.ClientPersonInfo where
  toTType' (Domain.Types.ClientPersonInfo.ClientPersonInfo {..}) = do
    Beam.ClientPersonInfoT
      { Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.rideCount = rideCount,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
