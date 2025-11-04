{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ClientPersonInfo where

import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.ClientPersonInfo
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ClientPersonInfo as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ClientPersonInfo.ClientPersonInfo -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ClientPersonInfo.ClientPersonInfo] -> m ())
createMany = traverse_ create

findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.ClientPersonInfo.ClientPersonInfo])
findAllByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPersonIdAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.ClientPersonInfo.ClientPersonInfo))
findByPersonIdAndVehicleCategory personId vehicleCategory = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory]]

updateHasTakenValidRideCount ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory -> m ())
updateHasTakenValidRideCount rideCount personId vehicleCategory = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.rideCount rideCount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ClientPersonInfo.ClientPersonInfo -> m (Maybe Domain.Types.ClientPersonInfo.ClientPersonInfo))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ClientPersonInfo.ClientPersonInfo -> m ())
updateByPrimaryKey (Domain.Types.ClientPersonInfo.ClientPersonInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientId (Kernel.Types.Id.getId <$> clientId),
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
