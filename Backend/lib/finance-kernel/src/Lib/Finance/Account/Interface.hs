{-
  Finance.Account.Interface

  Input types for account operations.
  The actual operations are in Lib.Finance.Account.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Account.Interface
  ( AccountInput (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (Currency)
import Lib.Finance.Domain.Types.Account (AccountCategory, AccountType)

-- | Input for creating an account
data AccountInput = AccountInput
  { accountType :: AccountType,
    accountCategory :: AccountCategory,
    ownerType :: Text,
    ownerId :: Text,
    currency :: Currency,
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic)
