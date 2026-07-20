{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.ReconciliationSummary where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
import qualified Lib.Finance.Reconciliation.Types
import Tools.Beam.UtilsTH

data ReconciliationSummaryT f = ReconciliationSummaryT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    disputeAmountTotal :: (B.C f Kernel.Types.Common.HighPrecMoney),
    domain :: (B.C f Lib.Finance.Reconciliation.Types.Domain),
    errorMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    matchRate :: (B.C f Kernel.Prelude.Text),
    matchedRecords :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    reconciliationDate :: (B.C f Kernel.Prelude.UTCTime),
    source :: (B.C f Lib.Finance.Reconciliation.Types.DataSource),
    sourceTotal :: (B.C f Kernel.Types.Common.HighPrecMoney),
    status :: (B.C f Lib.Finance.Domain.Types.ReconciliationSummary.JobStatus),
    target :: (B.C f Lib.Finance.Reconciliation.Types.DataSource),
    targetTotal :: (B.C f Kernel.Types.Common.HighPrecMoney),
    totalDiscrepancies :: (B.C f Kernel.Prelude.Int),
    totalRecords :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    varianceAmount :: (B.C f Kernel.Types.Common.HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table ReconciliationSummaryT where
  data PrimaryKey ReconciliationSummaryT f = ReconciliationSummaryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReconciliationSummaryId . id

type ReconciliationSummary = ReconciliationSummaryT Identity

$(enableKVPG (''ReconciliationSummaryT) [('id)] [[('reconciliationDate)]])

$(mkTableInstancesGenericSchema (''ReconciliationSummaryT) "finance_reconciliation_summary")
