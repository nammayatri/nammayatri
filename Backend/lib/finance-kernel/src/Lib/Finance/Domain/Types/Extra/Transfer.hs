{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.Extra.Transfer where

import Data.Aeson
import Data.OpenApi hiding (name)
import Kernel.Prelude
import Kernel.Types.Common

-- Extra code goes here --

-- | Typed transfer metadata: FLOW artifacts only (rail ids, UTR, fee breakdown, which
--   earning refs this payout covers). Domain information never goes here — the
--   counterparty on the account is the identity. DB column stays json.
data TransferMetadata = TransferMetadata
  { utr :: Maybe Text,
    feeBreakdown :: Maybe FeeBreakdown,
    coveredRefs :: Maybe [CoveredRef],
    reason :: Maybe Text,
    -- | rail-specific blob (batch id, fulfillment method, ...), opaque to the kernel
    railExtra :: Maybe Value
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSchema)

instance FromJSON TransferMetadata where
  parseJSON = genericParseJSON $ defaultOptions {omitNothingFields = True}

instance ToJSON TransferMetadata where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

emptyTransferMetadata :: TransferMetadata
emptyTransferMetadata = TransferMetadata Nothing Nothing Nothing Nothing Nothing

-- | How much of the transfer each reference type accounts for — lets a domain answer
--   "what did this payout cover" without re-deriving it from ledger legs.
data CoveredRef = CoveredRef
  { refType :: Text,
    amount :: HighPrecMoney
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeeBreakdown = FeeBreakdown
  { fixedFee :: Maybe HighPrecMoney,
    percentageFee :: Maybe HighPrecMoney
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
