{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.BulkUserCohortMappingRun (module Lib.IncentiveJourney.Storage.Queries.BulkUserCohortMappingRun, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow
import qualified Lib.IncentiveJourney.Storage.Beam.BulkUserCohortMappingRun as Beam
import Lib.IncentiveJourney.Storage.Queries.BulkUserCohortMappingRunExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun -> m ())
create = createWithKV

createMany :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun] -> m ())
createMany = traverse_ create

findById ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun -> m (Maybe Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantOperatingCityId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity -> m [Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun])
findByMerchantOperatingCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Desc Beam.createdAt) limit offset

findByPrimaryKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun -> m (Maybe Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun -> m ())
updateByPrimaryKey (Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchSize batchSize,
      Se.Set Beam.currentSchedulerJobId currentSchedulerJobId,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.fileOffset fileOffset,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rescheduleDelaySeconds rescheduleDelaySeconds,
      Se.Set Beam.rowsInserted rowsInserted,
      Se.Set Beam.rowsSkipped rowsSkipped,
      Se.Set Beam.s3FilePath s3FilePath,
      Se.Set Beam.scheduledAt scheduledAt,
      Se.Set Beam.status status,
      Se.Set Beam.totalRows totalRows,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
