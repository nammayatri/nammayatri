{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSCancellationConfig where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.FRFSCancellationConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSCancellationConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.FRFS.Enums.VehicleCategory -> m ([Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig]))
findAllByMerchantOpCityAndVehicleCategory merchantOperatingCityId vehicleCategory = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig -> m (Maybe Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig -> m ())
updateByPrimaryKey (Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cancellationChargeType cancellationChargeType,
      Se.Set Beam.cancellationChargeValue cancellationChargeValue,
      Se.Set Beam.currency currency,
      Se.Set Beam.maxMinutesBeforeDeparture maxMinutesBeforeDeparture,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.minMinutesBeforeDeparture minMinutesBeforeDeparture,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSCancellationConfig Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig where
  fromTType' (Beam.FRFSCancellationConfigT {..}) = do
    pure $
      Just
        Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig
          { cancellationChargeType = cancellationChargeType,
            cancellationChargeValue = cancellationChargeValue,
            createdAt = createdAt,
            currency = currency,
            id = Kernel.Types.Id.Id id,
            maxMinutesBeforeDeparture = maxMinutesBeforeDeparture,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minMinutesBeforeDeparture = minMinutesBeforeDeparture,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.FRFSCancellationConfig Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig where
  toTType' (Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig {..}) = do
    Beam.FRFSCancellationConfigT
      { Beam.cancellationChargeType = cancellationChargeType,
        Beam.cancellationChargeValue = cancellationChargeValue,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxMinutesBeforeDeparture = maxMinutesBeforeDeparture,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minMinutesBeforeDeparture = minMinutesBeforeDeparture,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
