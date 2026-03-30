{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TDSDistributionPdfFile (module Storage.Queries.TDSDistributionPdfFile, module ReExport) where

import qualified Domain.Types.TDSDistributionPdfFile
import qualified Domain.Types.TDSDistributionRecord
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TDSDistributionPdfFile as Beam
import Storage.Queries.TDSDistributionPdfFileExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile] -> m ())
createMany = traverse_ create

findAllByTdsDistributionRecordId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord) -> m ([Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile]))
findAllByTdsDistributionRecordId tdsDistributionRecordId = do findAllWithKV [Se.Is Beam.tdsDistributionRecordId $ Se.Eq (Kernel.Types.Id.getId <$> tdsDistributionRecordId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m (Maybe Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m (Maybe Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile -> m ())
updateByPrimaryKey (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fileName fileName,
      Se.Set Beam.s3FilePath s3FilePath,
      Se.Set Beam.tdsDistributionRecordId (Kernel.Types.Id.getId <$> tdsDistributionRecordId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
