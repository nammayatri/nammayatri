{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RewardUnlock where

import qualified Domain.Types.RewardUnlock
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RewardUnlock as Beam

instance FromTType' Beam.RewardUnlock Domain.Types.RewardUnlock.RewardUnlock where
  fromTType' (Beam.RewardUnlockT {..}) = do
    pure $
      Just
        Domain.Types.RewardUnlock.RewardUnlock
          { campaignId = Kernel.Types.Id.Id campaignId,
            claimedAt = claimedAt,
            cohortId = Kernel.Types.Id.Id cohortId,
            couponCode = couponCode,
            couponSource = couponSource,
            couponValidTill = couponValidTill,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            personId = Kernel.Types.Id.Id personId,
            reclaimedAt = reclaimedAt,
            redeemedAt = redeemedAt,
            status = status,
            unlockedAt = unlockedAt,
            updatedAt = updatedAt,
            viewedAt = viewedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.RewardUnlock Domain.Types.RewardUnlock.RewardUnlock where
  toTType' (Domain.Types.RewardUnlock.RewardUnlock {..}) = do
    Beam.RewardUnlockT
      { Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.claimedAt = claimedAt,
        Beam.cohortId = Kernel.Types.Id.getId cohortId,
        Beam.couponCode = couponCode,
        Beam.couponSource = couponSource,
        Beam.couponValidTill = couponValidTill,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.reclaimedAt = reclaimedAt,
        Beam.redeemedAt = redeemedAt,
        Beam.status = status,
        Beam.unlockedAt = unlockedAt,
        Beam.updatedAt = updatedAt,
        Beam.viewedAt = viewedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
