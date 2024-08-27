{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PayoutConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutConfig.PayoutConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PayoutConfig.PayoutConfig] -> m ())
createMany = traverse_ create

findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> Domain.Types.PayoutConfig.PayoutEntity -> m (Maybe Domain.Types.PayoutConfig.PayoutConfig))
findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity merchantOperatingCityId isPayoutEnabled payoutEntity = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.isPayoutEnabled $ Se.Eq isPayoutEnabled,
          Se.Is Beam.payoutEntity $ Se.Eq payoutEntity
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.PayoutConfig.PayoutConfig))
findByPrimaryKey merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutConfig.PayoutConfig -> m ())
updateByPrimaryKey (Domain.Types.PayoutConfig.PayoutConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchLimit batchLimit,
      Se.Set Beam.isPayoutEnabled isPayoutEnabled,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.orderType orderType,
      Se.Set Beam.payoutEntity payoutEntity,
      Se.Set Beam.remark remark,
      Se.Set Beam.timeDiff (Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

instance FromTType' Beam.PayoutConfig Domain.Types.PayoutConfig.PayoutConfig where
  fromTType' (Beam.PayoutConfigT {..}) = do
    pure $
      Just
        Domain.Types.PayoutConfig.PayoutConfig
          { batchLimit = batchLimit,
            isPayoutEnabled = isPayoutEnabled,
            maxRetryCount = maxRetryCount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            orderType = orderType,
            payoutEntity = payoutEntity,
            remark = remark,
            timeDiff = Kernel.Utils.Common.secondsToNominalDiffTime timeDiff,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutConfig Domain.Types.PayoutConfig.PayoutConfig where
  toTType' (Domain.Types.PayoutConfig.PayoutConfig {..}) = do
    Beam.PayoutConfigT
      { Beam.batchLimit = batchLimit,
        Beam.isPayoutEnabled = isPayoutEnabled,
        Beam.maxRetryCount = maxRetryCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.orderType = orderType,
        Beam.payoutEntity = payoutEntity,
        Beam.remark = remark,
        Beam.timeDiff = Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
