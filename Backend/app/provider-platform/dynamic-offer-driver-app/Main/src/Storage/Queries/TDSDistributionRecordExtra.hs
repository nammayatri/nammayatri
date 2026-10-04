module Storage.Queries.TDSDistributionRecordExtra where

import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.TDSDistributionBatch (TDSDistributionBatch)
import Domain.Types.TDSDistributionRecord (TDSDistributionRecord, TDSDistributionStatus)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.TDSDistributionRecord as Beam
import Storage.Queries.TDSDistributionRecord ()

findAllByStatusWithLimit ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id MerchantOperatingCity ->
  TDSDistributionStatus ->
  m [TDSDistributionRecord]
findAllByStatusWithLimit limit offset merchantOperatingCityId status =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)
        ]
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset

-- | A batch's records in the given statuses, oldest first.
findAllByBatchIdAndStatusesWithLimit ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id TDSDistributionBatch ->
  [TDSDistributionStatus] ->
  Int ->
  m [TDSDistributionRecord]
findAllByBatchIdAndStatusesWithLimit batchId statuses limit =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.batchId $ Se.Eq (Just batchId.getId),
          Se.Is Beam.status $ Se.In statuses
        ]
    ]
    (Se.Asc Beam.createdAt)
    (Just limit)
    Nothing
