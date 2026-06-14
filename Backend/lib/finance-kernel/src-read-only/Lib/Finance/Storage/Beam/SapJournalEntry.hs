{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.SapJournalEntry where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import Tools.Beam.UtilsTH

data SapJournalEntryT f = SapJournalEntryT
  { batchId :: (B.C f Kernel.Prelude.Text),
    belnr :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    blart :: (B.C f Kernel.Prelude.Text),
    bldat :: (B.C f Kernel.Prelude.Text),
    budat :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    description :: (B.C f Kernel.Prelude.Text),
    gjahr :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    glName :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    glNumber :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    rawResponse :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sapMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Lib.Finance.Domain.Types.SapJournalEntry.JournalEntryStatus),
    totalCreditAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    totalDebitAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    transactionCount :: (B.C f Kernel.Prelude.Int),
    transactionType :: (B.C f Lib.Finance.Domain.Types.SapJournalEntry.TransactionType),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SapJournalEntryT where
  data PrimaryKey SapJournalEntryT f = SapJournalEntryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SapJournalEntryId . id

type SapJournalEntry = SapJournalEntryT Identity

$(enableKVPG (''SapJournalEntryT) [('id)] [[('batchId)], [('transactionType)]])

$(mkTableInstancesGenericSchema (''SapJournalEntryT) "sap_journal_entry")
