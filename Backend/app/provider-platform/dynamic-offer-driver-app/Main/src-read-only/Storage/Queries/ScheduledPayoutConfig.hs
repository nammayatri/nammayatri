{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ScheduledPayoutConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ScheduledPayoutConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Common
import qualified Sequelize as Se
import qualified Storage.Beam.ScheduledPayoutConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByMerchantOpCityIdAndIsEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig])
findAllByMerchantOpCityIdAndIsEnabled merchantOperatingCityId isEnabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.isEnabled $ Se.Eq isEnabled
        ]
    ]

findByMerchantOpCityIdAndCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Lib.Payment.Domain.Types.Common.EntityName -> m (Maybe Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig))
findByMerchantOpCityIdAndCategory merchantOperatingCityId payoutCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.payoutCategory $ Se.Eq payoutCategory
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Lib.Payment.Domain.Types.Common.EntityName -> m (Maybe Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig))
findByPrimaryKey merchantOperatingCityId payoutCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.payoutCategory $ Se.Eq payoutCategory
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig -> m ())
updateByPrimaryKey (Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchSize batchSize,
      Se.Set Beam.dayOfMonth dayOfMonth,
      Se.Set Beam.dayOfWeek dayOfWeek,
      Se.Set Beam.defaultPayoutRail defaultPayoutRail,
      Se.Set Beam.discontinuedAt discontinuedAt,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.isEnabled isEnabled,
      Se.Set Beam.itemsPerBatch itemsPerBatch,
      Se.Set Beam.maxRetriesPerDriver maxRetriesPerDriver,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.minimumPayoutAmount minimumPayoutAmount,
      Se.Set Beam.orderType orderType,
      Se.Set Beam.pausedAt pausedAt,
      Se.Set Beam.remark remark,
      Se.Set Beam.settlementGatedEligibility settlementGatedEligibility,
      Se.Set Beam.timeDiffFromUtc timeDiffFromUtc,
      Se.Set Beam.timeOfDay timeOfDay,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.payoutCategory $ Se.Eq payoutCategory]]

instance FromTType' Beam.ScheduledPayoutConfig Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig where
  fromTType' (Beam.ScheduledPayoutConfigT {..}) = do
    pure $
      Just
        Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig
          { batchSize = batchSize,
            createdAt = createdAt,
            dayOfMonth = dayOfMonth,
            dayOfWeek = dayOfWeek,
            defaultPayoutRail = defaultPayoutRail,
            discontinuedAt = discontinuedAt,
            frequency = frequency,
            isEnabled = isEnabled,
            itemsPerBatch = itemsPerBatch,
            maxRetriesPerDriver = maxRetriesPerDriver,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minimumPayoutAmount = minimumPayoutAmount,
            orderType = orderType,
            pausedAt = pausedAt,
            payoutCategory = payoutCategory,
            remark = remark,
            settlementGatedEligibility = settlementGatedEligibility,
            timeDiffFromUtc = timeDiffFromUtc,
            timeOfDay = timeOfDay,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.ScheduledPayoutConfig Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig where
  toTType' (Domain.Types.ScheduledPayoutConfig.ScheduledPayoutConfig {..}) = do
    Beam.ScheduledPayoutConfigT
      { Beam.batchSize = batchSize,
        Beam.createdAt = createdAt,
        Beam.dayOfMonth = dayOfMonth,
        Beam.dayOfWeek = dayOfWeek,
        Beam.defaultPayoutRail = defaultPayoutRail,
        Beam.discontinuedAt = discontinuedAt,
        Beam.frequency = frequency,
        Beam.isEnabled = isEnabled,
        Beam.itemsPerBatch = itemsPerBatch,
        Beam.maxRetriesPerDriver = maxRetriesPerDriver,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minimumPayoutAmount = minimumPayoutAmount,
        Beam.orderType = orderType,
        Beam.pausedAt = pausedAt,
        Beam.payoutCategory = payoutCategory,
        Beam.remark = remark,
        Beam.settlementGatedEligibility = settlementGatedEligibility,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.timeOfDay = timeOfDay,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
