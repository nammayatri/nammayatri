{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconciliationSummary where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data ReconciliationSummary = ReconciliationSummary
  { createdAt :: Kernel.Prelude.UTCTime,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary,
    matchRate :: Kernel.Prelude.Text,
    matchedRecords :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    reconciliationDate :: Kernel.Prelude.UTCTime,
    reconciliationType :: Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationType,
    sourceTotal :: Kernel.Types.Common.HighPrecMoney,
    status :: Lib.Finance.Domain.Types.ReconciliationSummary.JobStatus,
    targetTotal :: Kernel.Types.Common.HighPrecMoney,
    totalDiscrepancies :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    varianceAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic)

data JobStatus = COMPLETED | FAILED | IN_PROGRESS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconciliationStatus = MATCHED | HIGHER_IN_TARGET | LOWER_IN_TARGET | MISSING_IN_TARGET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconciliationType = DSR_VS_LEDGER | DSR_VS_SUBSCRIPTION | DSSR_VS_SUBSCRIPTION deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''JobStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ReconciliationStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ReconciliationType))
