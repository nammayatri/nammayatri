{-
  Finance.Ledger.Interface

  Input types for ledger operations.
  The actual operations are in Lib.Finance.Ledger.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Ledger.Interface
  ( LedgerEntryInput (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id)
import Lib.Finance.Domain.Types.Account (Account)
import Lib.Finance.Domain.Types.LedgerEntry (EntryStatus, EntryType)

-- | Input for creating a ledger entry
data LedgerEntryInput = LedgerEntryInput
  { fromAccountId :: Id Account,
    toAccountId :: Id Account,
    amount :: HighPrecMoney,
    currency :: Currency,
    entryType :: EntryType,
    status :: EntryStatus,
    ownerType :: Text, -- "DRIVER", "FLEET_OWNER", "PLATFORM"
    ownerId :: Text,
    referenceType :: Text,
    referenceId :: Text,
    metadata :: Maybe Value,
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic)
