{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FeedbackBadge where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FeedbackBadge
import qualified Storage.Beam.FeedbackBadge as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.FeedbackBadge Domain.Types.FeedbackBadge.FeedbackBadge
    where fromTType' (Beam.FeedbackBadgeT {..}) = do pure $ Just Domain.Types.FeedbackBadge.FeedbackBadge{badge = badge,
                                                                                                          badgeCount = badgeCount,
                                                                                                          badgeKey = badgeKey,
                                                                                                          createdAt = createdAt,
                                                                                                          id = Kernel.Types.Id.Id id,
                                                                                                          riderId = Kernel.Types.Id.Id riderId,
                                                                                                          updatedAt = updatedAt,
                                                                                                          merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                          merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId}
instance ToTType' Beam.FeedbackBadge Domain.Types.FeedbackBadge.FeedbackBadge
    where toTType' (Domain.Types.FeedbackBadge.FeedbackBadge {..}) = do Beam.FeedbackBadgeT{Beam.badge = badge,
                                                                                            Beam.badgeCount = badgeCount,
                                                                                            Beam.badgeKey = badgeKey,
                                                                                            Beam.createdAt = createdAt,
                                                                                            Beam.id = Kernel.Types.Id.getId id,
                                                                                            Beam.riderId = Kernel.Types.Id.getId riderId,
                                                                                            Beam.updatedAt = updatedAt,
                                                                                            Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                            Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId}



