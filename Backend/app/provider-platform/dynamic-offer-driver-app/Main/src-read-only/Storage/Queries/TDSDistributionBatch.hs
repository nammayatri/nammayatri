{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TDSDistributionBatch (module Storage.Queries.TDSDistributionBatch, module ReExport) where

import qualified Domain.Types.TDSDistributionBatch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TDSDistributionBatch as Beam
import Storage.Queries.TDSDistributionBatchExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TDSDistributionBatch.TDSDistributionBatch] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m (Maybe Domain.Types.TDSDistributionBatch.TDSDistributionBatch))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateConfirmed ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionBatch.TDSDistributionBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m ())
updateConfirmed status confirmedAt confirmedById confirmedByName id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.confirmedAt confirmedAt,
      Se.Set Beam.confirmedById confirmedById,
      Se.Set Beam.confirmedByName confirmedByName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionBatch.TDSDistributionBatchStatus -> Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusAndCompletedAt ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionBatch.TDSDistributionBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m ())
updateStatusAndCompletedAt status completedAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.completedAt completedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusAndValidatedAt ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionBatch.TDSDistributionBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m ())
updateStatusAndValidatedAt status validatedAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.validatedAt validatedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m (Maybe Domain.Types.TDSDistributionBatch.TDSDistributionBatch))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionBatch.TDSDistributionBatch -> m ())
updateByPrimaryKey (Domain.Types.TDSDistributionBatch.TDSDistributionBatch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.completedAt completedAt,
      Se.Set Beam.confirmedAt confirmedAt,
      Se.Set Beam.confirmedById confirmedById,
      Se.Set Beam.confirmedByName confirmedByName,
      Se.Set Beam.financialYear financialYear,
      Se.Set Beam.folderName folderName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.quarter quarter,
      Se.Set Beam.status status,
      Se.Set Beam.totalFiles totalFiles,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.uploadedById uploadedById,
      Se.Set Beam.uploadedByName uploadedByName,
      Se.Set Beam.validatedAt validatedAt
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
