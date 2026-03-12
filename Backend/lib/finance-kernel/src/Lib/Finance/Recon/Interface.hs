{-
  Finance.Recon.Interface

  Domain-agnostic input types for PPF reconciliation processing.
  The caller (domain code) constructs these from protocol-specific types
  and provides a fare lookup callback.
-}
module Lib.Finance.Recon.Interface
  ( ReconInput (..),
    ReconOrderInput (..),
    ReconSettlementInput (..),
    FareLookup,
    ReconResult (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

-- | Callback the caller provides to look up the actual fare for a given order.
-- Returns the actual fare amount if found, Nothing if the order is unknown.
type FareLookup m = Text -> m (Maybe HighPrecMoney)

-- | Top-level recon request input (domain-agnostic).
data ReconInput = ReconInput
  { collectorSubscriberId :: Text,
    receiverSubscriberId :: Text,
    domain :: Text,
    currency :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    orderEntries :: [ReconOrderInput]
  }
  deriving (Generic)

-- | Individual order entry from a recon request.
data ReconOrderInput = ReconOrderInput
  { orderId :: Text,
    orderAmountSettled :: HighPrecMoney,
    settlements :: [ReconSettlementInput]
  }
  deriving (Generic)

-- | Settlement details from a recon order entry.
data ReconSettlementInput = ReconSettlementInput
  { settlementId :: Maybe Text,
    paymentId :: Maybe Text,
    status :: Maybe Text,
    amount :: Maybe HighPrecMoney,
    commission :: Maybe HighPrecMoney,
    withholdingAmount :: Maybe HighPrecMoney,
    tds :: Maybe HighPrecMoney,
    tcs :: Maybe HighPrecMoney,
    settlementRefNo :: Maybe Text
  }
  deriving (Generic)

-- | Result of processing a single recon order entry.
data ReconResult = ReconResult
  { accord :: Bool,
    expectedAmount :: Maybe HighPrecMoney,
    message :: Maybe Text
  }
  deriving (Generic)
