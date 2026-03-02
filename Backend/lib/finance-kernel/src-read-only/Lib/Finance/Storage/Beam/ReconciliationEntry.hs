{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.ReconciliationEntry where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.ReconciliationEntry

data ReconciliationEntryT f = ReconciliationEntryT
  { actualLedgerValue :: (B.C f Kernel.Types.Common.HighPrecMoney),
    bookingId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    dcoId :: (B.C f Kernel.Prelude.Text),
    expectedDsrValue :: (B.C f Kernel.Types.Common.HighPrecMoney),
    financeComponent :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconciliationEntry.FinanceComponent)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    mismatchReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    mode :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconciliationEntry.RideMode)),
    reconStatus :: (B.C f Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationStatus),
    reconciliationDate :: (B.C f Kernel.Prelude.UTCTime),
    reconciliationType :: (B.C f Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationType),
    sourceDetails :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Lib.Finance.Domain.Types.ReconciliationEntry.RideStatus),
    summaryId :: (B.C f Kernel.Prelude.Text),
    targetDetails :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    timestamp :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    variance :: (B.C f Kernel.Types.Common.HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table ReconciliationEntryT where
  data PrimaryKey ReconciliationEntryT f = ReconciliationEntryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReconciliationEntryId . id

type ReconciliationEntry = ReconciliationEntryT Identity

$(enableKVPG (''ReconciliationEntryT) [('id)] [[('bookingId)], [('dcoId)], [('reconStatus)], [('summaryId)]])

$(mkTableInstancesGenericSchema (''ReconciliationEntryT) "finance_reconciliation_entry")
