{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.LedgerEntry where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry

data LedgerEntryT f = LedgerEntryT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    entryNumber :: (B.C f Kernel.Prelude.Int),
    entryType :: (B.C f Lib.Finance.Domain.Types.LedgerEntry.EntryType),
    fromAccountId :: (B.C f Kernel.Prelude.Text),
    fromEndingBalance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    fromStartingBalance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    metadata :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    referenceId :: (B.C f Kernel.Prelude.Text),
    referenceType :: (B.C f Kernel.Prelude.Text),
    reversalOf :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    settledAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Lib.Finance.Domain.Types.LedgerEntry.EntryStatus),
    timestamp :: (B.C f Kernel.Prelude.UTCTime),
    toAccountId :: (B.C f Kernel.Prelude.Text),
    toEndingBalance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    toStartingBalance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    voidReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table LedgerEntryT where
  data PrimaryKey LedgerEntryT f = LedgerEntryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = LedgerEntryId . id

type LedgerEntry = LedgerEntryT Identity

$(enableKVPG (''LedgerEntryT) [('id)] [[('fromAccountId)], [('referenceId)], [('toAccountId)]])

$(mkTableInstancesGenericSchema (''LedgerEntryT) "finance_ledger_entry")
