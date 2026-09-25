{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data BulkUserCohortMappingRun = BulkUserCohortMappingRun
  { batchSize :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currentSchedulerJobId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fileOffset :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRun,
    merchantId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity,
    rescheduleDelaySeconds :: Kernel.Prelude.Int,
    rowsInserted :: Kernel.Prelude.Int,
    rowsSkipped :: Kernel.Prelude.Int,
    s3FilePath :: Kernel.Prelude.Text,
    scheduledAt :: Kernel.Prelude.UTCTime,
    status :: Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun.BulkUserCohortMappingRunStatus,
    totalRows :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BulkUserCohortMappingRunStatus = Scheduled | Running | Succeeded | Failed | Cancelled deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''BulkUserCohortMappingRunStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''BulkUserCohortMappingRunStatus)
