{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Image where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Image
import qualified Storage.Beam.Image as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.Image Domain.Types.Image.Image
    where fromTType' (Beam.ImageT {..}) = do pure $ Just Domain.Types.Image.Image{documentExpiry = documentExpiry,
                                                                                  failureReason = failureReason,
                                                                                  id = Kernel.Types.Id.Id id,
                                                                                  imageType = imageType,
                                                                                  merchantId = Kernel.Types.Id.Id merchantId,
                                                                                  merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                  personId = Kernel.Types.Id.Id personId,
                                                                                  rcId = rcId,
                                                                                  reviewerEmail = reviewerEmail,
                                                                                  s3Path = s3Path,
                                                                                  verificationStatus = verificationStatus,
                                                                                  workflowTransactionId = workflowTransactionId,
                                                                                  createdAt = createdAt,
                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.Image Domain.Types.Image.Image
    where toTType' (Domain.Types.Image.Image {..}) = do Beam.ImageT{Beam.documentExpiry = documentExpiry,
                                                                    Beam.failureReason = failureReason,
                                                                    Beam.id = Kernel.Types.Id.getId id,
                                                                    Beam.imageType = imageType,
                                                                    Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                    Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                    Beam.personId = Kernel.Types.Id.getId personId,
                                                                    Beam.rcId = rcId,
                                                                    Beam.reviewerEmail = reviewerEmail,
                                                                    Beam.s3Path = s3Path,
                                                                    Beam.verificationStatus = verificationStatus,
                                                                    Beam.workflowTransactionId = workflowTransactionId,
                                                                    Beam.createdAt = createdAt,
                                                                    Beam.updatedAt = updatedAt}



