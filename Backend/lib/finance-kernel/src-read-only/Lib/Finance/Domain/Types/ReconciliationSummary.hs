{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconciliationSummary where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Reconciliation.Types
import qualified Tools.Beam.UtilsTH

data ReconciliationSummary = ReconciliationSummary
  { createdAt :: Kernel.Prelude.UTCTime,
    disputeAmountTotal :: Kernel.Types.Common.HighPrecMoney,
    domain :: Lib.Finance.Reconciliation.Types.Domain,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary,
    matchRate :: Kernel.Prelude.Text,
    matchedRecords :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    reconciliationDate :: Kernel.Prelude.UTCTime,
    source :: Lib.Finance.Reconciliation.Types.DataSource,
    sourceTotal :: Kernel.Types.Common.HighPrecMoney,
    status :: Lib.Finance.Domain.Types.ReconciliationSummary.JobStatus,
    target :: Lib.Finance.Reconciliation.Types.DataSource,
    targetTotal :: Kernel.Types.Common.HighPrecMoney,
    totalDiscrepancies :: Kernel.Prelude.Int,
    totalRecords :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    varianceAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic)

data JobStatus = COMPLETED | FAILED | IN_PROGRESS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''JobStatus))
