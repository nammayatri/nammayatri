{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.OnDemand.Types.Recon where

import qualified BecknV2.OnDemand.Types as Spec
import Data.Aeson
import Kernel.Prelude

-- | BECKN recon request - sent by Collector (BAP) to Receiver (BPP) / ONDC
data ReconReq = ReconReq
  { reconReqContext :: Spec.Context,
    reconReqMessage :: ReconMessage
  }
  deriving (Show, Eq, Generic)

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

-- | Recon message containing list of order reconciliation entries
data ReconMessage = ReconMessage
  { reconMessageOrderRecons :: [OrderRecon]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReconMessage where
  parseJSON = genericParseJSON optionsReconMessage

instance ToJSON ReconMessage where
  toJSON = genericToJSON optionsReconMessage

optionsReconMessage :: Options
optionsReconMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "reconMessageOrderRecons" -> "order_recons"
        _ -> s
    }

-- | Individual order reconciliation entry
data OrderRecon = OrderRecon
  { orderReconOrderId :: Text,
    orderReconTransactionId :: Text,
    orderReconPaymentTransactionId :: Maybe Text,
    orderReconPaymentReference :: Maybe Text,
    orderReconOrderAmount :: ReconAmount,
    orderReconPaymentAmount :: ReconAmount,
    orderReconSellerShare :: ReconAmount,
    orderReconBuyerAppCommission :: ReconAmount,
    orderReconSettlement :: Maybe ReconSettlement,
    orderReconOrderStatus :: Text,
    orderReconPaymentStatus :: Text,
    orderReconSettlementStatus :: Text,
    orderReconTimestamps :: ReconTimestamps,
    orderReconWithholdingAmount :: Maybe ReconAmount,
    orderReconTds :: Maybe ReconAmount,
    orderReconTcs :: Maybe ReconAmount,
    orderReconNetworkFee :: Maybe ReconAmount,
    orderReconGstAmount :: Maybe ReconAmount
  }
  deriving (Show, Eq, Generic)

instance FromJSON OrderRecon where
  parseJSON = genericParseJSON optionsOrderRecon

instance ToJSON OrderRecon where
  toJSON = genericToJSON optionsOrderRecon

optionsOrderRecon :: Options
optionsOrderRecon =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "orderReconOrderId" -> "order_id"
        "orderReconTransactionId" -> "transaction_id"
        "orderReconPaymentTransactionId" -> "payment_transaction_id"
        "orderReconPaymentReference" -> "payment_reference"
        "orderReconOrderAmount" -> "order_amount"
        "orderReconPaymentAmount" -> "payment_amount"
        "orderReconSellerShare" -> "seller_share"
        "orderReconBuyerAppCommission" -> "buyer_app_commission"
        "orderReconSettlement" -> "settlement"
        "orderReconOrderStatus" -> "order_status"
        "orderReconPaymentStatus" -> "payment_status"
        "orderReconSettlementStatus" -> "settlement_status"
        "orderReconTimestamps" -> "timestamps"
        "orderReconWithholdingAmount" -> "withholding_amount"
        "orderReconTds" -> "tds"
        "orderReconTcs" -> "tcs"
        "orderReconNetworkFee" -> "network_fee"
        "orderReconGstAmount" -> "gst_amount"
        _ -> s
    }

-- | Monetary amount with currency
data ReconAmount = ReconAmount
  { reconAmountCurrency :: Text,
    reconAmountValue :: Text
  }
  deriving (Show, Eq, Generic)

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
        _ -> s
    }

-- | Settlement details within a recon entry
data ReconSettlement = ReconSettlement
  { reconSettlementId :: Maybe Text,
    reconSettlementAmount :: ReconAmount,
    reconSettlementRefNo :: Maybe Text,
    reconSettlementStatus :: Text,
    reconSettlementCommission :: Maybe ReconAmount,
    reconSettlementWithholdingAmount :: Maybe ReconAmount,
    reconSettlementTds :: Maybe ReconAmount,
    reconSettlementTcs :: Maybe ReconAmount
  }
  deriving (Show, Eq, Generic)

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
        "reconSettlementAmount" -> "amount"
        "reconSettlementRefNo" -> "settlement_ref_no"
        "reconSettlementStatus" -> "status"
        "reconSettlementCommission" -> "commission"
        "reconSettlementWithholdingAmount" -> "withholding_amount"
        "reconSettlementTds" -> "tds"
        "reconSettlementTcs" -> "tcs"
        _ -> s
    }

-- | Timestamps for the order lifecycle
data ReconTimestamps = ReconTimestamps
  { reconTimestampsCreatedAt :: Maybe Text,
    reconTimestampsFulfilledAt :: Maybe Text,
    reconTimestampsSettledAt :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReconTimestamps where
  parseJSON = genericParseJSON optionsReconTimestamps

instance ToJSON ReconTimestamps where
  toJSON = genericToJSON optionsReconTimestamps

optionsReconTimestamps :: Options
optionsReconTimestamps =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "reconTimestampsCreatedAt" -> "created_at"
        "reconTimestampsFulfilledAt" -> "fulfilled_at"
        "reconTimestampsSettledAt" -> "settled_at"
        _ -> s
    }

-- | BECKN on_recon response - sent by Receiver (BPP) back to Collector (BAP)
data OnReconReq = OnReconReq
  { onReconReqContext :: Spec.Context,
    onReconReqMessage :: OnReconMessage
  }
  deriving (Show, Eq, Generic)

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

-- | on_recon message containing list of order recon responses
data OnReconMessage = OnReconMessage
  { onReconMessageOrderRecons :: [OrderReconResponse]
  }
  deriving (Show, Eq, Generic)

instance FromJSON OnReconMessage where
  parseJSON = genericParseJSON optionsOnReconMessage

instance ToJSON OnReconMessage where
  toJSON = genericToJSON optionsOnReconMessage

optionsOnReconMessage :: Options
optionsOnReconMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "onReconMessageOrderRecons" -> "order_recons"
        _ -> s
    }

-- | Individual order reconciliation response
data OrderReconResponse = OrderReconResponse
  { orderReconResponseOrderId :: Text,
    orderReconResponseStatus :: Text, -- ACCEPTED | DISPUTED | SETTLED
    orderReconResponseMessage :: Maybe Text,
    orderReconResponseSettlementRefNo :: Maybe Text,
    orderReconResponseUpdatedAt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON OrderReconResponse where
  parseJSON = genericParseJSON optionsOrderReconResponse

instance ToJSON OrderReconResponse where
  toJSON = genericToJSON optionsOrderReconResponse

optionsOrderReconResponse :: Options
optionsOrderReconResponse =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> case s of
        "orderReconResponseOrderId" -> "order_id"
        "orderReconResponseStatus" -> "status"
        "orderReconResponseMessage" -> "message"
        "orderReconResponseSettlementRefNo" -> "settlement_ref_no"
        "orderReconResponseUpdatedAt" -> "updated_at"
        _ -> s
    }
