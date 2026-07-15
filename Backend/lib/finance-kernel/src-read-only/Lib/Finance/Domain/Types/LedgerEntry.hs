{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.LedgerEntry where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Core.Types
import qualified Lib.Finance.Domain.Types.Account
import qualified Tools.Beam.UtilsTH

data LedgerEntry = LedgerEntry
  { amount :: Kernel.Types.Common.HighPrecMoney,
    concernedIndividualId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    createdBy :: Kernel.Prelude.Maybe Lib.Finance.Core.Types.ActorType,
    createdById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    currency :: Kernel.Types.Common.Currency,
    entityReferenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityReferenceType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.LedgerEntry.EntityReferenceType,
    entryType :: Lib.Finance.Domain.Types.LedgerEntry.EntryType,
    fromAccountId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account,
    fromEndingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fromStartingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.LedgerEntry.LedgerEntryMetadata,
    reconciliationStatus :: Kernel.Prelude.Maybe Data.Aeson.Value,
    referenceId :: Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Text,
    reversalOf :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry),
    settledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementStatus :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.LedgerEntry.SettlementStatus,
    settlementTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Lib.Finance.Domain.Types.LedgerEntry.EntryStatus,
    timestamp :: Kernel.Prelude.UTCTime,
    toAccountId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account,
    toEndingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    toStartingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    updatedBy :: Kernel.Prelude.Maybe Lib.Finance.Core.Types.ActorType,
    updatedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    voidReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data EntityReferenceType = RefundRequest deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data EntryStatus = PENDING | DUE | SETTLED | VOIDED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data EntryType = Expense | Revenue | LiabilityCreated | LiabilitySettled | Reversal deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data LedgerEntryMetadata = LedgerEntryMetadata
  { d2cReferralEarnings :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    d2dReferralEarnings :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    dailyStatsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverPayable :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionAllocations :: Kernel.Prelude.Maybe [Lib.Finance.Domain.Types.LedgerEntry.SubscriptionCreditAllocation]
  }
  deriving (Generic, (Show), (Eq), (ToJSON), (FromJSON), (ToSchema))

data SettlementStatus = UNSETTLED | PROCESSING | PAID_OUT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SubscriptionCreditAllocation = SubscriptionCreditAllocation {amount :: Kernel.Types.Common.HighPrecMoney, subscriptionPurchaseId :: Kernel.Prelude.Text}
  deriving (Generic, (Show), (Eq), (ToJSON), (FromJSON), (ToSchema))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''EntityReferenceType))

$(mkHttpInstancesForEnum (''EntityReferenceType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''EntryStatus))

$(mkHttpInstancesForEnum (''EntryStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''EntryType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SettlementStatus))

$(mkHttpInstancesForEnum (''SettlementStatus))
