{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Stage where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Stage
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Stage as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Stage.Stage -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Stage.Stage] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> m ([Domain.Types.Stage.Stage]))
findByMerchantOperatingCityAndVehicleType merchantOperatingCityId vehicleType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]

findByMerchantOperatingCityAndVehicleTypeAndStageName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> Domain.Types.Stage.StageName -> m (Maybe Domain.Types.Stage.Stage))
findByMerchantOperatingCityAndVehicleTypeAndStageName merchantOperatingCityId vehicleType name = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.name $ Se.Eq name
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Stage.Stage -> m (Maybe Domain.Types.Stage.Stage))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Stage.Stage -> m ())
updateByPrimaryKey (Domain.Types.Stage.Stage {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.inputDataType inputDataType,
      Se.Set Beam.name name,
      Se.Set Beam.order order,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Stage Domain.Types.Stage.Stage where
  fromTType' (Beam.StageT {..}) = do
    pure $
      Just
        Domain.Types.Stage.Stage
          { id = Kernel.Types.Id.Id id,
            inputDataType = inputDataType,
            name = name,
            order = order,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Stage Domain.Types.Stage.Stage where
  toTType' (Domain.Types.Stage.Stage {..}) = do
    Beam.StageT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.inputDataType = inputDataType,
        Beam.name = name,
        Beam.order = order,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
