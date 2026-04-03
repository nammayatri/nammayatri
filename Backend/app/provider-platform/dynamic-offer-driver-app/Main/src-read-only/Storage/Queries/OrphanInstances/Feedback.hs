{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Feedback where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Feedback
import qualified Storage.Beam.Feedback as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.Feedback Domain.Types.Feedback.Feedback
    where fromTType' (Beam.FeedbackT {..}) = do pure $ Just Domain.Types.Feedback.Feedback{badge = badge,
                                                                                           badgeKey = badgeKey,
                                                                                           createdAt = createdAt,
                                                                                           driverId = Kernel.Types.Id.Id driverId,
                                                                                           id = Kernel.Types.Id.Id id,
                                                                                           rating = rating,
                                                                                           rideId = Kernel.Types.Id.Id rideId,
                                                                                           merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                           merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId}
instance ToTType' Beam.Feedback Domain.Types.Feedback.Feedback
    where toTType' (Domain.Types.Feedback.Feedback {..}) = do Beam.FeedbackT{Beam.badge = badge,
                                                                             Beam.badgeKey = badgeKey,
                                                                             Beam.createdAt = createdAt,
                                                                             Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                             Beam.id = Kernel.Types.Id.getId id,
                                                                             Beam.rating = rating,
                                                                             Beam.rideId = Kernel.Types.Id.getId rideId,
                                                                             Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                             Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId}



