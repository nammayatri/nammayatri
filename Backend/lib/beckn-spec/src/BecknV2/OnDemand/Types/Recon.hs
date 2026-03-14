{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | ONDC NTS RSF 2.0.0 compliant types for /recon and /on_recon APIs.
-- Spec ref: ONDC-Official/ONDC-NTS-Specifications branch release-RSF-2.0.0
module BecknV2.OnDemand.Types.Recon where

import qualified BecknV2.OnDemand.Types as Spec
import Data.Aeson
import Kernel.Prelude

-- ============================================================
-- Shared types
-- ============================================================

-- | Monetary amount with currency (used in recon)
data ReconAmount = ReconAmount
  { reconAmountCurrency :: Text,
    reconAmountValue :: Text,
    reconAmountDiffAmount :: Maybe Text
  }
  deriving (Eq, Generic)

instance FromJSON ReconAmount where
  parseJSON = genericParseJSON optionsReconAmount

instance ToJSON ReconAmount where
  toJSON = genericToJSON optionsReconAmount

optionsReconAmount :: Options
optionsReconAmount =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "reconAmountCurrency" -> "currency"
        "reconAmountValue" -> "value"
        "reconAmountDiffAmount" -> "diff_amount"
        _ -> s
    }

-- ============================================================
-- Recon request (BAP/Collector → BPP/Receiver)
-- ============================================================

-- | Top-level recon request
data ReconReq = ReconReq
  { reconReqContext :: Spec.Context,
    reconReqMessage :: ReconMessage
  }
  deriving (Eq, Generic)

instance FromJSON ReconReq where
  parseJSON = genericParseJSON optionsReconReq

instance ToJSON ReconReq where
  toJSON = genericToJSON optionsReconReq

optionsReconReq :: Options
optionsReconReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "reconReqContext" -> "context"
        "reconReqMessage" -> "message"
        _ -> s
    }

-- | Recon message containing order reconciliation entries
data ReconMessage = ReconMessage
  { reconMessageOrders :: [ReconOrder]
  }
  deriving (Eq, Generic)

instance FromJSON ReconMessage where
  parseJSON = genericParseJSON optionsReconMessage

instance ToJSON ReconMessage where
  toJSON = genericToJSON optionsReconMessage

optionsReconMessage :: Options
optionsReconMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "reconMessageOrders" -> "orders"
        _ -> s
    }

-- | Individual order in a recon request (per ONDC NTS RSF 2.0.0)
data ReconOrder = ReconOrder
  { reconOrderId :: Text,
    reconOrderAmount :: ReconAmount,
    reconOrderSettlements :: [ReconSettlement]
  }
  deriving (Eq, Generic)

instance FromJSON ReconOrder where
  parseJSON = genericParseJSON optionsReconOrder

instance ToJSON ReconOrder where
  toJSON = genericToJSON optionsReconOrder

optionsReconOrder :: Options
optionsReconOrder =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "reconOrderId" -> "id"
        "reconOrderAmount" -> "amount"
        "reconOrderSettlements" -> "settlements"
        _ -> s
    }

-- | Settlement entry within a recon order
data ReconSettlement = ReconSettlement
  { reconSettlementId :: Maybe Text,
    reconSettlementPaymentId :: Maybe Text,
    reconSettlementStatus :: Maybe Text,
    reconSettlementAmount :: Maybe ReconAmount,
    reconSettlementCommission :: Maybe ReconAmount,
    reconSettlementWithholdingAmount :: Maybe ReconAmount,
    reconSettlementTcs :: Maybe ReconAmount,
    reconSettlementTds :: Maybe ReconAmount,
    reconSettlementUpdatedAt :: Maybe Text,
    reconSettlementSettlementRefNo :: Maybe Text
  }
  deriving (Eq, Generic)

instance FromJSON ReconSettlement where
  parseJSON = genericParseJSON optionsReconSettlement

instance ToJSON ReconSettlement where
  toJSON = genericToJSON optionsReconSettlement

optionsReconSettlement :: Options
optionsReconSettlement =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "reconSettlementId" -> "id"
        "reconSettlementPaymentId" -> "payment_id"
        "reconSettlementStatus" -> "status"
        "reconSettlementAmount" -> "amount"
        "reconSettlementCommission" -> "commission"
        "reconSettlementWithholdingAmount" -> "withholding_amount"
        "reconSettlementTcs" -> "tcs"
        "reconSettlementTds" -> "tds"
        "reconSettlementUpdatedAt" -> "updated_at"
        "reconSettlementSettlementRefNo" -> "settlement_ref_no"
        _ -> s
    }

-- ============================================================
-- On_recon response (BPP/Receiver → BAP/Collector)
-- ============================================================

-- | Top-level on_recon response
data OnReconReq = OnReconReq
  { onReconReqContext :: Spec.Context,
    onReconReqMessage :: OnReconMessage
  }
  deriving (Eq, Generic)

instance FromJSON OnReconReq where
  parseJSON = genericParseJSON optionsOnReconReq

instance ToJSON OnReconReq where
  toJSON = genericToJSON optionsOnReconReq

optionsOnReconReq :: Options
optionsOnReconReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "onReconReqContext" -> "context"
        "onReconReqMessage" -> "message"
        _ -> s
    }

-- | On_recon message containing order recon responses
data OnReconMessage = OnReconMessage
  { onReconMessageOrders :: [OnReconOrder]
  }
  deriving (Eq, Generic)

instance FromJSON OnReconMessage where
  parseJSON = genericParseJSON optionsOnReconMessage

instance ToJSON OnReconMessage where
  toJSON = genericToJSON optionsOnReconMessage

optionsOnReconMessage :: Options
optionsOnReconMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "onReconMessageOrders" -> "orders"
        _ -> s
    }

-- | Individual order in an on_recon response (per ONDC NTS RSF 2.0.0)
data OnReconOrder = OnReconOrder
  { onReconOrderId :: Text,
    onReconOrderAmount :: ReconAmount,
    onReconOrderReconAccord :: Bool,
    onReconOrderSettlements :: [OnReconSettlement]
  }
  deriving (Eq, Generic)

instance FromJSON OnReconOrder where
  parseJSON = genericParseJSON optionsOnReconOrder

instance ToJSON OnReconOrder where
  toJSON = genericToJSON optionsOnReconOrder

optionsOnReconOrder :: Options
optionsOnReconOrder =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "onReconOrderId" -> "id"
        "onReconOrderAmount" -> "amount"
        "onReconOrderReconAccord" -> "recon_accord"
        "onReconOrderSettlements" -> "settlements"
        _ -> s
    }

-- | Settlement entry within an on_recon order (amounts have optional diff_value)
data OnReconSettlement = OnReconSettlement
  { onReconSettlementId :: Maybe Text,
    onReconSettlementPaymentId :: Maybe Text,
    onReconSettlementStatus :: Maybe Text,
    onReconSettlementDueDate :: Maybe Text,
    onReconSettlementAmount :: Maybe ReconAmount,
    onReconSettlementCommission :: Maybe ReconAmount,
    onReconSettlementWithholdingAmount :: Maybe ReconAmount,
    onReconSettlementTcs :: Maybe ReconAmount,
    onReconSettlementTds :: Maybe ReconAmount,
    onReconSettlementUpdatedAt :: Maybe Text,
    onReconSettlementSettlementRefNo :: Maybe Text
  }
  deriving (Eq, Generic)

instance FromJSON OnReconSettlement where
  parseJSON = genericParseJSON optionsOnReconSettlement

instance ToJSON OnReconSettlement where
  toJSON = genericToJSON optionsOnReconSettlement

optionsOnReconSettlement :: Options
optionsOnReconSettlement =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "onReconSettlementId" -> "id"
        "onReconSettlementPaymentId" -> "payment_id"
        "onReconSettlementStatus" -> "status"
        "onReconSettlementDueDate" -> "due_date"
        "onReconSettlementAmount" -> "amount"
        "onReconSettlementCommission" -> "commission"
        "onReconSettlementWithholdingAmount" -> "withholding_amount"
        "onReconSettlementTcs" -> "tcs"
        "onReconSettlementTds" -> "tds"
        "onReconSettlementUpdatedAt" -> "updated_at"
        "onReconSettlementSettlementRefNo" -> "settlement_ref_no"
        _ -> s
    }
