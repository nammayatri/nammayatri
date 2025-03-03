{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PayoutConfig where

import qualified Domain.Types.PayoutConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Storage.Beam.PayoutConfig as Beam

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
            referralProgramStartDate = referralProgramStartDate,
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
        Beam.referralProgramStartDate = referralProgramStartDate,
        Beam.referralRewardAmountPerRide = referralRewardAmountPerRide,
        Beam.remark = remark,
        Beam.thresholdPayoutAmountPerPerson = thresholdPayoutAmountPerPerson,
        Beam.timeDiff = Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
