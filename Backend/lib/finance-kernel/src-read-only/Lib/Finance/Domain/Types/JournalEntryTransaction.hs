{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.JournalEntryTransaction where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Core.Types
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Tools.Beam.UtilsTH

data JournalEntryTransaction = JournalEntryTransaction
  { createdAt :: Kernel.Prelude.UTCTime,
    createdBy :: Lib.Finance.Core.Types.ActorType,
    createdById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    creditAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    debitAmount :: Kernel.Types.Common.HighPrecMoney,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    referenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.JournalEntryTransaction.ReferenceType,
    sapBatchId :: Kernel.Prelude.Text,
    sapJournalEntryId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry,
    status :: Kernel.Prelude.Text,
    transactionType :: Lib.Finance.Domain.Types.SapJournalEntry.TransactionType,
    updatedAt :: Kernel.Prelude.UTCTime,
    updatedBy :: Lib.Finance.Core.Types.ActorType,
    updatedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic)

data ReferenceType = SubscriptionPurchase | Booking | Payout | TdsReimbursementRequest deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReferenceType)

$(mkHttpInstancesForEnum ''ReferenceType)
