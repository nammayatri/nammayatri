{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.PayoutConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.PayoutConfig
import qualified Storage.Beam.PayoutConfig as Beam
import qualified Kernel.Types.Id
import qualified Kernel.Utils.Common



instance FromTType' Beam.PayoutConfig Domain.Types.PayoutConfig.PayoutConfig
    where fromTType' (Beam.PayoutConfigT {..}) = do pure $ Just Domain.Types.PayoutConfig.PayoutConfig{batchLimit = batchLimit,
                                                                                                       coinRedemptionMinimumLimit = coinRedemptionMinimumLimit,
                                                                                                       d2dPayoutType = fromMaybe Domain.Types.PayoutConfig.NO_PAYOUT d2dPayoutType,
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
                                                                                                       referralRewardAmountPerRideForD2DPayout = referralRewardAmountPerRideForD2DPayout,
                                                                                                       remark = remark,
                                                                                                       thresholdPayoutAmountPerPerson = thresholdPayoutAmountPerPerson,
                                                                                                       timeDiff = Kernel.Utils.Common.secondsToNominalDiffTime timeDiff,
                                                                                                       vehicleCategory = vehicleCategory,
                                                                                                       vpaVerificationMode = fromMaybe Domain.Types.PayoutConfig.PAYMENT_BASED vpaVerificationMode,
                                                                                                       createdAt = createdAt,
                                                                                                       updatedAt = updatedAt}
instance ToTType' Beam.PayoutConfig Domain.Types.PayoutConfig.PayoutConfig
    where toTType' (Domain.Types.PayoutConfig.PayoutConfig {..}) = do Beam.PayoutConfigT{Beam.batchLimit = batchLimit,
                                                                                         Beam.coinRedemptionMinimumLimit = coinRedemptionMinimumLimit,
                                                                                         Beam.d2dPayoutType = Just d2dPayoutType,
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
                                                                                         Beam.referralRewardAmountPerRideForD2DPayout = referralRewardAmountPerRideForD2DPayout,
                                                                                         Beam.remark = remark,
                                                                                         Beam.thresholdPayoutAmountPerPerson = thresholdPayoutAmountPerPerson,
                                                                                         Beam.timeDiff = Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff,
                                                                                         Beam.vehicleCategory = vehicleCategory,
                                                                                         Beam.vpaVerificationMode = Just vpaVerificationMode,
                                                                                         Beam.createdAt = createdAt,
                                                                                         Beam.updatedAt = updatedAt}



