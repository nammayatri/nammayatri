{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.JournalEntryTransaction where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import Tools.Beam.UtilsTH

data JournalEntryTransactionT f = JournalEntryTransactionT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    createdBy :: (B.C f Kernel.Prelude.Text),
    creditAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    currency :: (B.C f Kernel.Types.Common.Currency),
    debitAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    description :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    sapBatchId :: (B.C f Kernel.Prelude.Text),
    sapJournalEntryId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Kernel.Prelude.Text),
    subscriptionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    transactionType :: (B.C f Lib.Finance.Domain.Types.SapJournalEntry.TransactionType),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table JournalEntryTransactionT where
  data PrimaryKey JournalEntryTransactionT f = JournalEntryTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JournalEntryTransactionId . id

type JournalEntryTransaction = JournalEntryTransactionT Identity

$(enableKVPG (''JournalEntryTransactionT) [('id)] [])

$(mkTableInstancesGenericSchema (''JournalEntryTransactionT) "journal_entry_transaction")
