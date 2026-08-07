{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ScheduledPayoutConfigExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import Kernel.Beam.Functions (findAllWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Payment.Domain.Types.Common as LPDTC
import qualified Sequelize as Se
import qualified Storage.Beam.ScheduledPayoutConfig as Beam
-- Brings the Beam<->Domain FromTType'/ToTType' instances into scope (defined in the read-only module).
import Storage.Queries.ScheduledPayoutConfig ()

-- | ConfigPilot fallback: narrows by whichever dimensions are set (isEnabled, payoutCategory).
findByDimensions ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Bool ->
  Maybe LPDTC.EntityName ->
  m [DSPC.ScheduledPayoutConfig]
findByDimensions (Id merchantOperatingCityId) mbIsEnabled mbPayoutCategory =
  findAllWithKV
    [ Se.And $
        [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]
          <> [Se.Is Beam.isEnabled $ Se.Eq b | Just b <- [mbIsEnabled]]
          <> [Se.Is Beam.payoutCategory $ Se.Eq c | Just c <- [mbPayoutCategory]]
    ]
