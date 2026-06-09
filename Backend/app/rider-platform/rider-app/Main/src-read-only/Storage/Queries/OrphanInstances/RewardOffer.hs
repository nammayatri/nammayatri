{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RewardOffer where

import qualified Domain.Types.RewardOffer
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RewardOffer as Beam

instance FromTType' Beam.RewardOffer Domain.Types.RewardOffer.RewardOffer where
  fromTType' (Beam.RewardOfferT {..}) = do
    pure $
      Just
        Domain.Types.RewardOffer.RewardOffer
          { active = active,
            description = description,
            displayOrder = displayOrder,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            imageUrl = imageUrl,
            logicDomain = logicDomain,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            milestoneTarget = milestoneTarget,
            requiredTags = requiredTags,
            title = title,
            triggerEvent = triggerEvent,
            validFrom = validFrom,
            validTill = validTill,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RewardOffer Domain.Types.RewardOffer.RewardOffer where
  toTType' (Domain.Types.RewardOffer.RewardOffer {..}) = do
    Beam.RewardOfferT
      { Beam.active = active,
        Beam.description = description,
        Beam.displayOrder = displayOrder,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.imageUrl = imageUrl,
        Beam.logicDomain = logicDomain,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.milestoneTarget = milestoneTarget,
        Beam.requiredTags = requiredTags,
        Beam.title = title,
        Beam.triggerEvent = triggerEvent,
        Beam.validFrom = validFrom,
        Beam.validTill = validTill,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
