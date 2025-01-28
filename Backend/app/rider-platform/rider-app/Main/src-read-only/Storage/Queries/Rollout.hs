{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Rollout where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Rollout
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Rollout as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Rollout.Rollout -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Rollout.Rollout] -> m ())
createMany = traverse_ create

deleteByVersionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Rollout.Rollout -> m ())
deleteByVersionId id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> m [Domain.Types.Rollout.Rollout])
findAllByMerchantOperatingCityAndVehicleType merchantOperatingCityId vehicleType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]

findByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> m (Maybe Domain.Types.Rollout.Rollout))
findByMerchantOperatingCityAndVehicleType merchantOperatingCityId vehicleType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]

updateByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> m ())
updateByMerchantOperatingCityAndVehicleType percentage merchantOperatingCityId vehicleType = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.percentage percentage, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Rollout.Rollout -> m (Maybe Domain.Types.Rollout.Rollout))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Rollout.Rollout -> m ())
updateByPrimaryKey (Domain.Types.Rollout.Rollout {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.inputDataType inputDataType,
      Se.Set Beam.percentage percentage,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.versionTag versionTag,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Rollout Domain.Types.Rollout.Rollout where
  fromTType' (Beam.RolloutT {..}) = do
    pure $
      Just
        Domain.Types.Rollout.Rollout
          { id = Kernel.Types.Id.Id id,
            inputDataType = inputDataType,
            percentage = percentage,
            vehicleType = vehicleType,
            versionTag = versionTag,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Rollout Domain.Types.Rollout.Rollout where
  toTType' (Domain.Types.Rollout.Rollout {..}) = do
    Beam.RolloutT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.inputDataType = inputDataType,
        Beam.percentage = percentage,
        Beam.vehicleType = vehicleType,
        Beam.versionTag = versionTag,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
