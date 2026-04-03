{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.MediaFileDocument where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.MediaFileDocument
import qualified Storage.Beam.MediaFileDocument as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.MediaFileDocument Domain.Types.MediaFileDocument.MediaFileDocument
    where fromTType' (Beam.MediaFileDocumentT {..}) = do pure $ Just Domain.Types.MediaFileDocument.MediaFileDocument{creatorId = Kernel.Types.Id.Id creatorId,
                                                                                                                      fileHash = fileHash,
                                                                                                                      id = Kernel.Types.Id.Id id,
                                                                                                                      mediaFileDocumentType = mediaFileDocumentType,
                                                                                                                      merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                      merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                      rcId = Kernel.Types.Id.Id rcId,
                                                                                                                      s3Path = s3Path,
                                                                                                                      status = status,
                                                                                                                      uploadLink = uploadLink,
                                                                                                                      createdAt = createdAt,
                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.MediaFileDocument Domain.Types.MediaFileDocument.MediaFileDocument
    where toTType' (Domain.Types.MediaFileDocument.MediaFileDocument {..}) = do Beam.MediaFileDocumentT{Beam.creatorId = Kernel.Types.Id.getId creatorId,
                                                                                                        Beam.fileHash = fileHash,
                                                                                                        Beam.id = Kernel.Types.Id.getId id,
                                                                                                        Beam.mediaFileDocumentType = mediaFileDocumentType,
                                                                                                        Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                        Beam.rcId = Kernel.Types.Id.getId rcId,
                                                                                                        Beam.s3Path = s3Path,
                                                                                                        Beam.status = status,
                                                                                                        Beam.uploadLink = uploadLink,
                                                                                                        Beam.createdAt = createdAt,
                                                                                                        Beam.updatedAt = updatedAt}



