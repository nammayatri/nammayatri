{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FeedbackBadge where

import qualified Domain.Types.FeedbackBadge
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FeedbackBadge as Beam

instance FromTType' Beam.FeedbackBadge Domain.Types.FeedbackBadge.FeedbackBadge where
  fromTType' (Beam.FeedbackBadgeT {..}) = do
    pure $
      Just
        Domain.Types.FeedbackBadge.FeedbackBadge
          { badge = badge,
            badgeCount = badgeCount,
            badgeKey = badgeKey,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.FeedbackBadge Domain.Types.FeedbackBadge.FeedbackBadge where
  toTType' (Domain.Types.FeedbackBadge.FeedbackBadge {..}) = do
    Beam.FeedbackBadgeT
      { Beam.badge = badge,
        Beam.badgeCount = badgeCount,
        Beam.badgeKey = badgeKey,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
