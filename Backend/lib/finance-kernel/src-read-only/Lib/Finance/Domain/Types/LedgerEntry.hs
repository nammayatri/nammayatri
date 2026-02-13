{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.LedgerEntry where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.Account

data LedgerEntry = LedgerEntry
  { amount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    entryNumber :: Kernel.Prelude.Int,
    entryType :: Lib.Finance.Domain.Types.LedgerEntry.EntryType,
    fromAccountId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account,
    fromEndingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fromStartingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    referenceId :: Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Text,
    reversalOf :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry),
    settledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Lib.Finance.Domain.Types.LedgerEntry.EntryStatus,
    timestamp :: Kernel.Prelude.UTCTime,
    toAccountId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account,
    toEndingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    toStartingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    voidReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data EntryStatus = PENDING | DUE | SETTLED | VOIDED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data EntryType = Expense | Revenue | LiabilityCreated | LiabilitySettled | Reversal deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''EntryType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''EntryStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''EntryStatus))
