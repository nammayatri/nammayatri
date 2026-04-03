{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.TDSDistributionPdfFile (module Storage.Queries.TDSDistributionPdfFile, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.TDSDistributionPdfFileExtra as ReExport
import qualified Domain.Types.TDSDistributionPdfFile
import qualified Storage.Beam.TDSDistributionPdfFile as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.TDSDistributionRecord
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile] -> m ())
createMany = traverse_ create
findAllByTdsDistributionRecordId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                    (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord) -> m ([Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile]))
findAllByTdsDistributionRecordId tdsDistributionRecordId = do findAllWithKV [Se.Is Beam.tdsDistributionRecordId $ Se.Eq (Kernel.Types.Id.getId <$> tdsDistributionRecordId)]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
            (Kernel.Types.Id.Id Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m (Maybe Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m (Maybe Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m ())
updateByPrimaryKey (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile {..}) = do {_now <- getCurrentTime;
                                                                                           updateWithKV [Se.Set Beam.fileName fileName,
                                                                                                         Se.Set Beam.s3FilePath s3FilePath,
                                                                                                         Se.Set Beam.tdsDistributionRecordId (Kernel.Types.Id.getId <$> tdsDistributionRecordId),
                                                                                                         Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



