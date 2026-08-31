{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.BulkUserCohortMappingRun where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun
import qualified Lib.IncentiveJourney.Storage.Beam.BulkUserCohortMappingRun as Beam

instance FromTType' Beam.BulkUserCohortMappingRun Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun where
  fromTType' (Beam.BulkUserCohortMappingRunT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun
          { batchSize = batchSize,
            createdAt = createdAt,
            currentSchedulerJobId = currentSchedulerJobId,
            errorMessage = errorMessage,
            fileOffset = fileOffset,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rescheduleDelaySeconds = rescheduleDelaySeconds,
            rowsInserted = rowsInserted,
            rowsSkipped = rowsSkipped,
            s3FilePath = s3FilePath,
            scheduledAt = scheduledAt,
            status = status,
            totalRows = totalRows,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BulkUserCohortMappingRun Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun where
  toTType' (Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun {..}) = do
    Beam.BulkUserCohortMappingRunT
      { Beam.batchSize = batchSize,
        Beam.createdAt = createdAt,
        Beam.currentSchedulerJobId = currentSchedulerJobId,
        Beam.errorMessage = errorMessage,
        Beam.fileOffset = fileOffset,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rescheduleDelaySeconds = rescheduleDelaySeconds,
        Beam.rowsInserted = rowsInserted,
        Beam.rowsSkipped = rowsSkipped,
        Beam.s3FilePath = s3FilePath,
        Beam.scheduledAt = scheduledAt,
        Beam.status = status,
        Beam.totalRows = totalRows,
        Beam.updatedAt = updatedAt
      }
