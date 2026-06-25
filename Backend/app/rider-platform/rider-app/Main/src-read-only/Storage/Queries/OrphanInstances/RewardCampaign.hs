{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RewardCampaign where

import qualified Domain.Types.RewardCampaign
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RewardCampaign as Beam

instance FromTType' Beam.RewardCampaign Domain.Types.RewardCampaign.RewardCampaign where
  fromTType' (Beam.RewardCampaignT {..}) = do
    pure $
      Just
        Domain.Types.RewardCampaign.RewardCampaign
          { claimMode = claimMode,
            couponSourceType = couponSourceType,
            couponTemplate = couponTemplate,
            createdAt = createdAt,
            createdBy = createdBy,
            description = description,
            displayOrder = displayOrder,
            endsAt = endsAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            reclaimPolicy = Kernel.Prelude.identity reclaimPolicy,
            redemptionTargetType = redemptionTargetType,
            redemptionTargetUrl = redemptionTargetUrl,
            sponsorLogoUrl = sponsorLogoUrl,
            sponsorName = sponsorName,
            sponsorType = sponsorType,
            startsAt = startsAt,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RewardCampaign Domain.Types.RewardCampaign.RewardCampaign where
  toTType' (Domain.Types.RewardCampaign.RewardCampaign {..}) = do
    Beam.RewardCampaignT
      { Beam.claimMode = claimMode,
        Beam.couponSourceType = couponSourceType,
        Beam.couponTemplate = couponTemplate,
        Beam.createdAt = createdAt,
        Beam.createdBy = createdBy,
        Beam.description = description,
        Beam.displayOrder = displayOrder,
        Beam.endsAt = endsAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.reclaimPolicy = Kernel.Prelude.identity reclaimPolicy,
        Beam.redemptionTargetType = redemptionTargetType,
        Beam.redemptionTargetUrl = redemptionTargetUrl,
        Beam.sponsorLogoUrl = sponsorLogoUrl,
        Beam.sponsorName = sponsorName,
        Beam.sponsorType = sponsorType,
        Beam.startsAt = startsAt,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
