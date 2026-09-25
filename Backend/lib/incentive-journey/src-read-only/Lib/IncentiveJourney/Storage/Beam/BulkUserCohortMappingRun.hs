{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.BulkUserCohortMappingRun where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun

data BulkUserCohortMappingRunT f = BulkUserCohortMappingRunT
  { batchSize :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currentSchedulerJobId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    errorMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fileOffset :: B.C f Kernel.Prelude.Int,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    rescheduleDelaySeconds :: B.C f Kernel.Prelude.Int,
    rowsInserted :: B.C f Kernel.Prelude.Int,
    rowsSkipped :: B.C f Kernel.Prelude.Int,
    s3FilePath :: B.C f Kernel.Prelude.Text,
    scheduledAt :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRunStatus,
    totalRows :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BulkUserCohortMappingRunT where
  data PrimaryKey BulkUserCohortMappingRunT f = BulkUserCohortMappingRunId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BulkUserCohortMappingRunId . id

type BulkUserCohortMappingRun = BulkUserCohortMappingRunT Identity

$(enableKVPG ''BulkUserCohortMappingRunT ['id] [])

$(mkTableInstancesGenericSchema ''BulkUserCohortMappingRunT "bulk_user_cohort_mapping_run")
