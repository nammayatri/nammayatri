{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.JournalEntryTransaction where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Tools.Beam.UtilsTH

data JournalEntryTransaction = JournalEntryTransaction
  { createdAt :: Kernel.Prelude.UTCTime,
    createdBy :: Kernel.Prelude.Text,
    creditAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    debitAmount :: Kernel.Types.Common.HighPrecMoney,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    sapBatchId :: Kernel.Prelude.Text,
    sapJournalEntryId :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    subscriptionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transactionType :: Lib.Finance.Domain.Types.SapJournalEntry.TransactionType,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
