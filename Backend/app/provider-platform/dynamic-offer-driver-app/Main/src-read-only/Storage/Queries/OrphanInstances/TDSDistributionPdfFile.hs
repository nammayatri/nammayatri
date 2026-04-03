{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.TDSDistributionPdfFile where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.TDSDistributionPdfFile
import qualified Storage.Beam.TDSDistributionPdfFile as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.TDSDistributionPdfFile Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile
    where fromTType' (Beam.TDSDistributionPdfFileT {..}) = do pure $ Just Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile{createdAt = createdAt,
                                                                                                                                     fileName = fileName,
                                                                                                                                     id = Kernel.Types.Id.Id id,
                                                                                                                                     s3FilePath = s3FilePath,
                                                                                                                                     tdsDistributionRecordId = Kernel.Types.Id.Id <$> tdsDistributionRecordId,
                                                                                                                                     updatedAt = updatedAt}
instance ToTType' Beam.TDSDistributionPdfFile Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile
    where toTType' (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile {..}) = do Beam.TDSDistributionPdfFileT{Beam.createdAt = createdAt,
                                                                                                                       Beam.fileName = fileName,
                                                                                                                       Beam.id = Kernel.Types.Id.getId id,
                                                                                                                       Beam.s3FilePath = s3FilePath,
                                                                                                                       Beam.tdsDistributionRecordId = Kernel.Types.Id.getId <$> tdsDistributionRecordId,
                                                                                                                       Beam.updatedAt = updatedAt}



