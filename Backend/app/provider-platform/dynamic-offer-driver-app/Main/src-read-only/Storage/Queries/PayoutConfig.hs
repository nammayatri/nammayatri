{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PayoutConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
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

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.PayoutConfig.PayoutConfig))
findByMerchantOperatingCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.PayoutConfig.PayoutConfig))
findByPrimaryKey merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutConfig.PayoutConfig -> m ())
updateByPrimaryKey (Domain.Types.PayoutConfig.PayoutConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchLimit batchLimit,
      Se.Set Beam.isPayoutEnabled isPayoutEnabled,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.payoutRegistrationAmount payoutRegistrationAmount,
      Se.Set Beam.referralRewardAmountPerRide referralRewardAmountPerRide,
      Se.Set Beam.thresholdPayoutAmountPerPerson thresholdPayoutAmountPerPerson,
      Se.Set Beam.timeDiff (Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff),
      Se.Set Beam.vehicleServiceTier vehicleServiceTier,
      Se.Set Beam.vehicleVariant vehicleVariant,
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
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            payoutRegistrationAmount = payoutRegistrationAmount,
            referralRewardAmountPerRide = referralRewardAmountPerRide,
            thresholdPayoutAmountPerPerson = thresholdPayoutAmountPerPerson,
            timeDiff = Kernel.Utils.Common.secondsToNominalDiffTime timeDiff,
            vehicleServiceTier = vehicleServiceTier,
            vehicleVariant = vehicleVariant,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutConfig Domain.Types.PayoutConfig.PayoutConfig where
  toTType' (Domain.Types.PayoutConfig.PayoutConfig {..}) = do
    Beam.PayoutConfigT
      { Beam.batchLimit = batchLimit,
        Beam.isPayoutEnabled = isPayoutEnabled,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.payoutRegistrationAmount = payoutRegistrationAmount,
        Beam.referralRewardAmountPerRide = referralRewardAmountPerRide,
        Beam.thresholdPayoutAmountPerPerson = thresholdPayoutAmountPerPerson,
        Beam.timeDiff = Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff,
        Beam.vehicleServiceTier = vehicleServiceTier,
        Beam.vehicleVariant = vehicleVariant,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
