{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.ReconciliationEntry where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Reconciliation.Types
import Tools.Beam.UtilsTH

data ReconciliationEntryT f = ReconciliationEntryT
  { actualAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    closeReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    component :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    domain :: (B.C f Lib.Finance.Reconciliation.Types.Domain),
    entityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    entityMeta :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    entryKey :: (B.C f Kernel.Prelude.Text),
    expectedAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    firstSeenAt :: (B.C f Kernel.Prelude.UTCTime),
    groupSourceTotal :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    groupTargetAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    groupTargetKey :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    mismatchReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    open :: (B.C f Kernel.Prelude.Bool),
    partyId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reconStatus :: (B.C f Lib.Finance.Reconciliation.Types.ReconciliationStatus),
    reconciliationDate :: (B.C f Kernel.Prelude.UTCTime),
    resolvedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    rrn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    settlementId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    settlementMode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    source :: (B.C f Lib.Finance.Reconciliation.Types.DataSource),
    sourceLifecycle :: (B.C f Lib.Finance.Reconciliation.Types.Lifecycle),
    sourceRecordId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    summaryId :: (B.C f Kernel.Prelude.Text),
    target :: (B.C f Lib.Finance.Reconciliation.Types.DataSource),
    targetRecordId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    timestamp :: (B.C f Kernel.Prelude.UTCTime),
    transactionDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    variance :: (B.C f Kernel.Types.Common.HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table ReconciliationEntryT where
  data PrimaryKey ReconciliationEntryT f = ReconciliationEntryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReconciliationEntryId . id

type ReconciliationEntry = ReconciliationEntryT Identity

$(enableKVPG (''ReconciliationEntryT) [('id)] [[('entityId)], [('entryKey)], [('groupTargetKey)], [('summaryId)]])

$(mkTableInstancesGenericSchema (''ReconciliationEntryT) "finance_reconciliation_entry")
