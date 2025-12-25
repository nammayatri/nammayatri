{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutConfigExtra where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutConfig as DPC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import Storage.Beam.PayoutConfig as Beam
import Storage.Queries.OrphanInstances.PayoutConfig

-- Extra code goes here --

updateConfigValues :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Common.PayoutConfigReq -> DPC.PayoutConfig -> Id DMOC.MerchantOperatingCity -> m ()
updateConfigValues req existingConfig (Id merchantOperatingCityId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isPayoutEnabled (fromMaybe existingConfig.isPayoutEnabled req.toEnable),
      Se.Set Beam.referralRewardAmountPerRide (fromMaybe existingConfig.referralRewardAmountPerRide req.referralRewardAmountPerRide),
      Se.Set Beam.payoutRegistrationFee (fromMaybe existingConfig.payoutRegistrationFee req.payoutRegistrationFee),
      Se.Set Beam.payoutRegistrationCgst (fromMaybe existingConfig.payoutRegistrationCgst req.payoutRegistrationCgst),
      Se.Set Beam.payoutRegistrationSgst (fromMaybe existingConfig.payoutRegistrationSgst req.payoutRegistrationSgst),
      Se.Set Beam.thresholdPayoutAmountPerPerson (fromMaybe existingConfig.thresholdPayoutAmountPerPerson req.thresholdPayoutAmountPerPerson),
      Se.Set Beam.remark (fromMaybe existingConfig.remark req.remark),
      Se.Set Beam.updatedAt now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId, Se.Is Beam.vehicleCategory $ Se.Eq req.vehicleCategory]]
