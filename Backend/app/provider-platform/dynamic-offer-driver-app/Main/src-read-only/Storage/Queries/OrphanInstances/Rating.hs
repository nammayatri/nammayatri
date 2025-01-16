{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Rating where

import qualified Domain.Types.Rating
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Rating as Beam

instance FromTType' Beam.Rating Domain.Types.Rating.Rating where
  fromTType' (Beam.RatingT {..}) = do
    pure $
      Just
        Domain.Types.Rating.Rating
          { createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            feedbackDetails = feedbackDetails,
            id = Kernel.Types.Id.Id id,
            isFavourite = isFavourite,
            isSafe = isSafe,
            issueId = issueId,
            mediaId = Kernel.Types.Id.Id <$> mediaId,
            ratingValue = ratingValue,
            rideId = Kernel.Types.Id.Id rideId,
            updatedAt = updatedAt,
            wasOfferedAssistance = wasOfferedAssistance,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Rating Domain.Types.Rating.Rating where
  toTType' (Domain.Types.Rating.Rating {..}) = do
    Beam.RatingT
      { Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.feedbackDetails = feedbackDetails,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isFavourite = isFavourite,
        Beam.isSafe = isSafe,
        Beam.issueId = issueId,
        Beam.mediaId = Kernel.Types.Id.getId <$> mediaId,
        Beam.ratingValue = ratingValue,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.updatedAt = updatedAt,
        Beam.wasOfferedAssistance = wasOfferedAssistance,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
