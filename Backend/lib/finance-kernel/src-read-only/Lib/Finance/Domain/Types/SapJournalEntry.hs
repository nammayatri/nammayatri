{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.SapJournalEntry where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Core.Types
import qualified Tools.Beam.UtilsTH

data SapJournalEntry = SapJournalEntry
  { batchId :: Kernel.Prelude.Text,
    belnr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blart :: Kernel.Prelude.Text,
    bldat :: Kernel.Prelude.Text,
    budat :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    createdBy :: Lib.Finance.Core.Types.ActorType,
    createdById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Text,
    gjahr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    glName :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    glNumber :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    periodEndTime :: Kernel.Prelude.UTCTime,
    periodStartTime :: Kernel.Prelude.UTCTime,
    rawResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sapMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Lib.Finance.Domain.Types.SapJournalEntry.JournalEntryStatus,
    totalCreditAmount :: Kernel.Types.Common.HighPrecMoney,
    totalDebitAmount :: Kernel.Types.Common.HighPrecMoney,
    transactionCount :: Kernel.Prelude.Int,
    transactionType :: Lib.Finance.Domain.Types.SapJournalEntry.TransactionType,
    updatedAt :: Kernel.Prelude.UTCTime,
    updatedBy :: Lib.Finance.Core.Types.ActorType,
    updatedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic)

data JournalEntryStatus = SUCCESS | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TransactionType = SubscriptionPurchase | Order | Refund | Chargeback | RevenueRecognition deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''JournalEntryStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TransactionType))
