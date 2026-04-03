{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Finance.Storage.Beam.ReconciliationEntry where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Lib.Finance.Domain.Types.ReconciliationEntry
import qualified Database.Beam as B



data ReconciliationEntryT f
    = ReconciliationEntryT {actualLedgerValue :: (B.C f Kernel.Types.Common.HighPrecMoney),
                            bookingId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            dcoId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
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
                            rrn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                            settlementId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            settlementMode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            sourceDetails :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            sourceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            status :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.ReconciliationEntry.RideStatus)),
                            summaryId :: (B.C f Kernel.Prelude.Text),
                            targetDetails :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            targetId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            timestamp :: (B.C f Kernel.Prelude.UTCTime),
                            transactionDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                            variance :: (B.C f Kernel.Types.Common.HighPrecMoney)}
    deriving (Generic, B.Beamable)
instance B.Table ReconciliationEntryT
    where data PrimaryKey ReconciliationEntryT f = ReconciliationEntryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = ReconciliationEntryId . id
type ReconciliationEntry = ReconciliationEntryT Identity

$(enableKVPG (''ReconciliationEntryT) [('id)] [[('bookingId)], [('dcoId)], [('reconStatus)], [('summaryId)]])

$(mkTableInstancesGenericSchema (''ReconciliationEntryT) "finance_reconciliation_entry")

