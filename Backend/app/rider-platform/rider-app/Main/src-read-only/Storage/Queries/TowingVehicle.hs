{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TowingVehicle where

import qualified Domain.Types.TowingVehicle
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TowingVehicle as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TowingVehicle.TowingVehicle -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TowingVehicle.TowingVehicle] -> m ()
createMany = traverse_ create

findAllByAgencyEntityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Text -> m [Domain.Types.TowingVehicle.TowingVehicle]
findAllByAgencyEntityId agencyEntityId = do findAllWithKV [Se.Is Beam.agencyEntityId $ Se.Eq agencyEntityId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.TowingVehicle.TowingVehicle -> m (Maybe Domain.Types.TowingVehicle.TowingVehicle)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.TowingVehicle.TowingVehicle -> m (Maybe Domain.Types.TowingVehicle.TowingVehicle)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TowingVehicle.TowingVehicle -> m ()
updateByPrimaryKey (Domain.Types.TowingVehicle.TowingVehicle {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agencyEntityId agencyEntityId,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TowingVehicle Domain.Types.TowingVehicle.TowingVehicle where
  fromTType' (Beam.TowingVehicleT {..}) = do
    pure $
      Just
        Domain.Types.TowingVehicle.TowingVehicle
          { agencyEntityId = agencyEntityId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            vehicleNumber = vehicleNumber,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TowingVehicle Domain.Types.TowingVehicle.TowingVehicle where
  toTType' (Domain.Types.TowingVehicle.TowingVehicle {..}) = do
    Beam.TowingVehicleT
      { Beam.agencyEntityId = agencyEntityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
