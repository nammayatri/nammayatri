{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RewardCohort where

import qualified Domain.Types.RewardCohort
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RewardCohort as Beam

instance FromTType' Beam.RewardCohort Domain.Types.RewardCohort.RewardCohort where
  fromTType' (Beam.RewardCohortT {..}) = do
    pure $
      Just
        Domain.Types.RewardCohort.RewardCohort
          { campaignId = Kernel.Types.Id.Id campaignId,
            couponValidityDays = couponValidityDays,
            createdAt = createdAt,
            description = description,
            displayOrder = displayOrder,
            eligibilityJsonLogic = Kernel.Prelude.identity eligibilityJsonLogic,
            id = Kernel.Types.Id.Id id,
            maxUnlocksPerCohort = maxUnlocksPerCohort,
            name = name,
            presentation = Kernel.Prelude.identity presentation,
            rewardImageUrl = rewardImageUrl,
            rewardTitle = rewardTitle,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.RewardCohort Domain.Types.RewardCohort.RewardCohort where
  toTType' (Domain.Types.RewardCohort.RewardCohort {..}) = do
    Beam.RewardCohortT
      { Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.couponValidityDays = couponValidityDays,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.displayOrder = displayOrder,
        Beam.eligibilityJsonLogic = Kernel.Prelude.identity eligibilityJsonLogic,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxUnlocksPerCohort = maxUnlocksPerCohort,
        Beam.name = name,
        Beam.presentation = Kernel.Prelude.identity presentation,
        Beam.rewardImageUrl = rewardImageUrl,
        Beam.rewardTitle = rewardTitle,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
