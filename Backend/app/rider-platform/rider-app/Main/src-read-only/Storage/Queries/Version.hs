{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Version where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.Extra.Rollout
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Version
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Version as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Version.Version -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Version.Version] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityAndVehicleTypeAndDataType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> Domain.Types.Extra.Rollout.RawDataType -> m [Domain.Types.Version.Version])
findAllByMerchantOperatingCityAndVehicleTypeAndDataType merchantOperatingCityId vehicleType inputDataType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.inputDataType $ Se.Eq inputDataType
        ]
    ]

findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> Domain.Types.Extra.Rollout.RawDataType -> m [Domain.Types.Version.Version])
findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType isReadyToApply merchantOperatingCityId vehicleType inputDataType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.isReadyToApply $ Se.Eq isReadyToApply,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.inputDataType $ Se.Eq inputDataType
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Version.Version -> m (Maybe Domain.Types.Version.Version))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Version.Version -> m ())
updateByPrimaryKey (Domain.Types.Version.Version {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.inputDataType inputDataType,
      Se.Set Beam.isReadyToApply isReadyToApply,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.versionTag versionTag,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Version Domain.Types.Version.Version where
  fromTType' (Beam.VersionT {..}) = do
    pure $
      Just
        Domain.Types.Version.Version
          { id = Kernel.Types.Id.Id id,
            inputDataType = inputDataType,
            isReadyToApply = isReadyToApply,
            vehicleType = vehicleType,
            versionTag = versionTag,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Version Domain.Types.Version.Version where
  toTType' (Domain.Types.Version.Version {..}) = do
    Beam.VersionT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.inputDataType = inputDataType,
        Beam.isReadyToApply = isReadyToApply,
        Beam.vehicleType = vehicleType,
        Beam.versionTag = versionTag,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
