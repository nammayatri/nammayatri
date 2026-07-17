{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.Extra.LedgerEntry where

import Data.Aeson
import Data.OpenApi -- hiding (Example, example, name, tags)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.TH

-- Extra code goes here --

-- | Typed ledger metadata (DB column stays JSON Value). Use this to keep known fields
-- structured and avoid opaque free-form JSON for sensitive / recon attribution data.
data LedgerEntryMetadata = LedgerEntryMetadata
  { d2cReferralEarnings :: Maybe HighPrecMoney,
    d2dReferralEarnings :: Maybe HighPrecMoney,
    dailyStatsId :: Maybe Text,
    driverPayable :: Maybe HighPrecMoney,
    payoutOrderId :: Maybe Text,
    reason :: Maybe Text,
    subscriptionAllocations :: Maybe [SubscriptionCreditAllocation]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSchema)

instance FromJSON LedgerEntryMetadata where
  parseJSON = genericParseJSON $ defaultOptions {omitNothingFields = True}

instance ToJSON LedgerEntryMetadata where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

data SubscriptionCreditAllocation = SubscriptionCreditAllocation
  { amount :: HighPrecMoney,
    subscriptionPurchaseId :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
