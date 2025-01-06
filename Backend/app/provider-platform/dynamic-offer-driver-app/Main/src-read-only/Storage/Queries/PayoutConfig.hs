{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PayoutConfig
import qualified Domain.Types.VehicleCategory
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

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.PayoutConfig.PayoutConfig])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByMerchantOpCityIdAndIsPayoutEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.PayoutConfig.PayoutConfig])
findByMerchantOpCityIdAndIsPayoutEnabled merchantOperatingCityId isPayoutEnabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.isPayoutEnabled $ Se.Eq isPayoutEnabled
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleCategory.VehicleCategory -> m (Maybe Domain.Types.PayoutConfig.PayoutConfig))
findByPrimaryKey merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutConfig.PayoutConfig -> m ())
updateByPrimaryKey (Domain.Types.PayoutConfig.PayoutConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchLimit batchLimit,
      Se.Set Beam.expand expand,
      Se.Set Beam.isPayoutEnabled isPayoutEnabled,
      Se.Set Beam.maxPayoutReferralForADay maxPayoutReferralForADay,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.orderType orderType,
      Se.Set Beam.payoutRegistrationCgst payoutRegistrationCgst,
      Se.Set Beam.payoutRegistrationFee payoutRegistrationFee,
      Se.Set Beam.payoutRegistrationSgst payoutRegistrationSgst,
      Se.Set Beam.referralRewardAmountPerRide referralRewardAmountPerRide,
      Se.Set Beam.remark remark,
      Se.Set Beam.thresholdPayoutAmountPerPerson thresholdPayoutAmountPerPerson,
      Se.Set Beam.timeDiff (Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory]]

instance FromTType' Beam.PayoutConfig Domain.Types.PayoutConfig.PayoutConfig where
  fromTType' (Beam.PayoutConfigT {..}) = do
    pure $
      Just
        Domain.Types.PayoutConfig.PayoutConfig
          { batchLimit = batchLimit,
            expand = expand,
            isPayoutEnabled = isPayoutEnabled,
            maxPayoutReferralForADay = maxPayoutReferralForADay,
            maxRetryCount = maxRetryCount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            orderType = orderType,
            payoutRegistrationCgst = payoutRegistrationCgst,
            payoutRegistrationFee = payoutRegistrationFee,
            payoutRegistrationSgst = payoutRegistrationSgst,
            referralRewardAmountPerRide = referralRewardAmountPerRide,
            remark = remark,
            thresholdPayoutAmountPerPerson = thresholdPayoutAmountPerPerson,
            timeDiff = Kernel.Utils.Common.secondsToNominalDiffTime timeDiff,
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutConfig Domain.Types.PayoutConfig.PayoutConfig where
  toTType' (Domain.Types.PayoutConfig.PayoutConfig {..}) = do
    Beam.PayoutConfigT
      { Beam.batchLimit = batchLimit,
        Beam.expand = expand,
        Beam.isPayoutEnabled = isPayoutEnabled,
        Beam.maxPayoutReferralForADay = maxPayoutReferralForADay,
        Beam.maxRetryCount = maxRetryCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.orderType = orderType,
        Beam.payoutRegistrationCgst = payoutRegistrationCgst,
        Beam.payoutRegistrationFee = payoutRegistrationFee,
        Beam.payoutRegistrationSgst = payoutRegistrationSgst,
        Beam.referralRewardAmountPerRide = referralRewardAmountPerRide,
        Beam.remark = remark,
        Beam.thresholdPayoutAmountPerPerson = thresholdPayoutAmountPerPerson,
        Beam.timeDiff = Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
