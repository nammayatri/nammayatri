{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Feedback where

import qualified Domain.Types.Feedback
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Feedback as Beam

instance FromTType' Beam.Feedback Domain.Types.Feedback.Feedback where
  fromTType' (Beam.FeedbackT {..}) = do
    pure $
      Just
        Domain.Types.Feedback.Feedback
          { badge = badge,
            badgeKey = badgeKey,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            rating = rating,
            rideId = Kernel.Types.Id.Id rideId,
            riderId = Kernel.Types.Id.Id riderId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Feedback Domain.Types.Feedback.Feedback where
  toTType' (Domain.Types.Feedback.Feedback {..}) = do
    Beam.FeedbackT
      { Beam.badge = badge,
        Beam.badgeKey = badgeKey,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rating = rating,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
