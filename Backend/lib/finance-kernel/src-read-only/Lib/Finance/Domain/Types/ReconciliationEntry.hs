{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconciliationEntry where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
import qualified Lib.Finance.Reconciliation.Types
import qualified Tools.Beam.UtilsTH

data ReconciliationEntry = ReconciliationEntry
  { actualAmount :: Kernel.Types.Common.HighPrecMoney,
    closeReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    component :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    domain :: Lib.Finance.Reconciliation.Types.Domain,
    entityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityMeta :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entryKey :: Kernel.Prelude.Text,
    expectedAmount :: Kernel.Types.Common.HighPrecMoney,
    firstSeenAt :: Kernel.Prelude.UTCTime,
    groupSourceTotal :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    groupTargetAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    groupTargetKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mismatchReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    open :: Kernel.Prelude.Bool,
    partyId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reconStatus :: Lib.Finance.Reconciliation.Types.ReconciliationStatus,
    reconciliationDate :: Kernel.Prelude.UTCTime,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Lib.Finance.Reconciliation.Types.DataSource,
    sourceLifecycle :: Lib.Finance.Reconciliation.Types.Lifecycle,
    sourceRecordId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    summaryId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary,
    target :: Lib.Finance.Reconciliation.Types.DataSource,
    targetRecordId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timestamp :: Kernel.Prelude.UTCTime,
    transactionDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    variance :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic)
