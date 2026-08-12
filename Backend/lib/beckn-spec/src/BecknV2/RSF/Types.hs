{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module BecknV2.RSF.Types
  ( ReceiverReconReq (..),
    OnReceiverReconReq (..),
    RSFContext (..),
    RSFOrderbookMessage (..),
    RSFOrderbook (..),
    RSFOrder (..),
    RSFPayment (..),
    RSFPaymentParams (..),
    RSFSettlementDetail (..),
    RSFPayerDetails (..),
    RSFMonetaryValue (..),
    RSFProvider (..),
    RSFProviderName (..),
    RSFOnReceiverReconOrder (..),
    RSFCounterpartyDiffAmount (..),
    RSFDiffMessage (..),
    RSFAck (..),
    RSFAckMessage (..),
    RSFAckResponse (..),
    RSFError (..),
    RSFOnReceiverReconMessage (..),
    RSFOnReceiverReconOrderbook (..),
    RSFOnReconSettlementDetail (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.Data (Data)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339)
import Prelude

data ReceiverReconReq = ReceiverReconReq
  { receiverReconReqContext :: RSFContext,
    receiverReconReqMessage :: RSFOrderbookMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ReceiverReconReq where
  parseJSON = genericParseJSON optionsReceiverReconReq

instance ToJSON ReceiverReconReq where
  toJSON = genericToJSON optionsReceiverReconReq

optionsReceiverReconReq :: Options
optionsReceiverReconReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("receiverReconReqContext", "context"),
        ("receiverReconReqMessage", "message")
      ]

data OnReceiverReconReq = OnReceiverReconReq
  { onReceiverReconReqContext :: RSFContext,
    onReceiverReconReqMessage :: RSFOnReceiverReconMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnReceiverReconReq where
  parseJSON = genericParseJSON optionsOnReceiverReconReq

instance ToJSON OnReceiverReconReq where
  toJSON = genericToJSON optionsOnReceiverReconReq

optionsOnReceiverReconReq :: Options
optionsOnReceiverReconReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onReceiverReconReqContext", "context"),
        ("onReceiverReconReqMessage", "message")
      ]

data RSFContext = RSFContext
  { rsfContextDomain :: Maybe Text,
    rsfContextCountry :: Maybe Text,
    rsfContextCity :: Maybe Text,
    rsfContextAction :: Maybe Text,
    rsfContextCoreVersion :: Maybe Text,
    rsfContextBapId :: Maybe Text,
    rsfContextBapUri :: Maybe Text,
    rsfContextBppId :: Maybe Text,
    rsfContextBppUri :: Maybe Text,
    rsfContextTransactionId :: Maybe Text,
    rsfContextMessageId :: Maybe Text,
    rsfContextTimestamp :: Maybe UTCTimeRFC3339,
    rsfContextTtl :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFContext where
  parseJSON = genericParseJSON optionsRSFContext

instance ToJSON RSFContext where
  toJSON = genericToJSON optionsRSFContext

optionsRSFContext :: Options
optionsRSFContext =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfContextDomain", "domain"),
        ("rsfContextCountry", "country"),
        ("rsfContextCity", "city"),
        ("rsfContextAction", "action"),
        ("rsfContextCoreVersion", "core_version"),
        ("rsfContextBapId", "bap_id"),
        ("rsfContextBapUri", "bap_uri"),
        ("rsfContextBppId", "bpp_id"),
        ("rsfContextBppUri", "bpp_uri"),
        ("rsfContextTransactionId", "transaction_id"),
        ("rsfContextMessageId", "message_id"),
        ("rsfContextTimestamp", "timestamp"),
        ("rsfContextTtl", "ttl")
      ]

newtype RSFOrderbookMessage = RSFOrderbookMessage
  { rsfOrderbookMessageOrderbook :: RSFOrderbook
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFOrderbookMessage where
  parseJSON = genericParseJSON optionsRSFOrderbookMessage

instance ToJSON RSFOrderbookMessage where
  toJSON = genericToJSON optionsRSFOrderbookMessage

optionsRSFOrderbookMessage :: Options
optionsRSFOrderbookMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table = [("rsfOrderbookMessageOrderbook", "orderbook")]

newtype RSFOrderbook = RSFOrderbook
  { rsfOrderbookOrders :: [RSFOrder]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFOrderbook where
  parseJSON = genericParseJSON optionsRSFOrderbook

instance ToJSON RSFOrderbook where
  toJSON = genericToJSON optionsRSFOrderbook

optionsRSFOrderbook :: Options
optionsRSFOrderbook =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table = [("rsfOrderbookOrders", "orders")]

data RSFOrder = RSFOrder
  { rsfOrderId :: Maybe Text,
    rsfOrderInvoiceNo :: Maybe Text,
    rsfOrderCollectorAppId :: Maybe Text,
    rsfOrderReceiverAppId :: Maybe Text,
    rsfOrderState :: Maybe Text,
    rsfOrderProvider :: Maybe RSFProvider,
    rsfOrderPayment :: Maybe RSFPayment,
    rsfOrderWithholdingTaxGst :: Maybe RSFMonetaryValue,
    rsfOrderWithholdingTaxTds :: Maybe RSFMonetaryValue,
    rsfOrderDeductionByCollector :: Maybe RSFMonetaryValue,
    rsfOrderPayerdetails :: Maybe RSFPayerDetails,
    rsfOrderSettlementReasonCode :: Maybe Text,
    rsfOrderTransactionId :: Maybe Text,
    rsfOrderSettlementId :: Maybe Text,
    rsfOrderSettlementReferenceNo :: Maybe Text,
    rsfOrderReconStatus :: Maybe Text,
    rsfOrderOrderReconStatus :: Maybe Text,
    rsfOrderCreatedAt :: Maybe UTCTimeRFC3339,
    rsfOrderUpdatedAt :: Maybe UTCTimeRFC3339
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFOrder where
  parseJSON = genericParseJSON optionsRSFOrder

instance ToJSON RSFOrder where
  toJSON = genericToJSON optionsRSFOrder

optionsRSFOrder :: Options
optionsRSFOrder =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfOrderId", "id"),
        ("rsfOrderInvoiceNo", "invoice_no"),
        ("rsfOrderCollectorAppId", "collector_app_id"),
        ("rsfOrderReceiverAppId", "receiver_app_id"),
        ("rsfOrderState", "state"),
        ("rsfOrderProvider", "provider"),
        ("rsfOrderPayment", "payment"),
        ("rsfOrderWithholdingTaxGst", "withholding_tax_gst"),
        ("rsfOrderWithholdingTaxTds", "withholding_tax_tds"),
        ("rsfOrderDeductionByCollector", "deduction_by_collector"),
        ("rsfOrderPayerdetails", "payerdetails"),
        ("rsfOrderSettlementReasonCode", "settlement_reason_code"),
        ("rsfOrderTransactionId", "transaction_id"),
        ("rsfOrderSettlementId", "settlement_id"),
        ("rsfOrderSettlementReferenceNo", "settlement_reference_no"),
        ("rsfOrderReconStatus", "recon_status"),
        ("rsfOrderOrderReconStatus", "order_recon_status"),
        ("rsfOrderCreatedAt", "created_at"),
        ("rsfOrderUpdatedAt", "updated_at")
      ]

data RSFPayment = RSFPayment
  { rsfPaymentUri :: Maybe Text,
    rsfPaymentTlMethod :: Maybe Text,
    rsfPaymentParams :: Maybe RSFPaymentParams,
    rsfPaymentType :: Maybe Text,
    rsfPaymentStatus :: Maybe Text,
    rsfPaymentCollectedBy :: Maybe Text,
    rsfPaymentCollectedByStatus :: Maybe Text,
    rsfPaymentBuyerAppFinderFeeType :: Maybe Text,
    rsfPaymentBuyerAppFinderFeeAmount :: Maybe Text,
    rsfPaymentWithholdingAmount :: Maybe Text,
    rsfPaymentWithholdingAmountStatus :: Maybe Text,
    rsfPaymentReturnWindow :: Maybe Text,
    rsfPaymentReturnWindowStatus :: Maybe Text,
    rsfPaymentSettlementBasis :: Maybe Text,
    rsfPaymentSettlementBasisStatus :: Maybe Text,
    rsfPaymentSettlementWindow :: Maybe Text,
    rsfPaymentSettlementWindowStatus :: Maybe Text,
    rsfPaymentSettlementDetails :: Maybe [RSFSettlementDetail]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFPayment where
  parseJSON = genericParseJSON optionsRSFPayment

instance ToJSON RSFPayment where
  toJSON = genericToJSON optionsRSFPayment

optionsRSFPayment :: Options
optionsRSFPayment =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfPaymentUri", "uri"),
        ("rsfPaymentTlMethod", "tl_method"),
        ("rsfPaymentParams", "params"),
        ("rsfPaymentType", "type"),
        ("rsfPaymentStatus", "status"),
        ("rsfPaymentCollectedBy", "collected_by"),
        ("rsfPaymentCollectedByStatus", "@ondc/org/collected_by_status"),
        ("rsfPaymentBuyerAppFinderFeeType", "@ondc/org/buyer_app_finder_fee_type"),
        ("rsfPaymentBuyerAppFinderFeeAmount", "@ondc/org/buyer_app_finder_fee_amount"),
        ("rsfPaymentWithholdingAmount", "@ondc/org/withholding_amount"),
        ("rsfPaymentWithholdingAmountStatus", "@ondc/org/withholding_amount_status"),
        ("rsfPaymentReturnWindow", "@ondc/org/return_window"),
        ("rsfPaymentReturnWindowStatus", "@ondc/org/return_window_status"),
        ("rsfPaymentSettlementBasis", "@ondc/org/settlement_basis"),
        ("rsfPaymentSettlementBasisStatus", "@ondc/org/settlement_basis_status"),
        ("rsfPaymentSettlementWindow", "@ondc/org/settlement_window"),
        ("rsfPaymentSettlementWindowStatus", "@ondc/org/settlement_window_status"),
        ("rsfPaymentSettlementDetails", "@ondc/org/settlement_details")
      ]

data RSFPaymentParams = RSFPaymentParams
  { rsfPaymentParamsTransactionId :: Maybe Text,
    rsfPaymentParamsTransactionStatus :: Maybe Text,
    rsfPaymentParamsAmount :: Maybe Text,
    rsfPaymentParamsCurrency :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFPaymentParams where
  parseJSON = genericParseJSON optionsRSFPaymentParams

instance ToJSON RSFPaymentParams where
  toJSON = genericToJSON optionsRSFPaymentParams

optionsRSFPaymentParams :: Options
optionsRSFPaymentParams =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfPaymentParamsTransactionId", "transaction_id"),
        ("rsfPaymentParamsTransactionStatus", "transaction_status"),
        ("rsfPaymentParamsAmount", "amount"),
        ("rsfPaymentParamsCurrency", "currency")
      ]

data RSFSettlementDetail = RSFSettlementDetail
  { rsfSettlementDetailCounterparty :: Maybe Text,
    rsfSettlementDetailPhase :: Maybe Text,
    rsfSettlementDetailAmount :: Maybe Double,
    rsfSettlementDetailType :: Maybe Text,
    rsfSettlementDetailBankAccountNo :: Maybe Text,
    rsfSettlementDetailIfscCode :: Maybe Text,
    rsfSettlementDetailUpiAddress :: Maybe Text,
    rsfSettlementDetailBankName :: Maybe Text,
    rsfSettlementDetailBranchName :: Maybe Text,
    rsfSettlementDetailBeneficiaryAddress :: Maybe Text,
    rsfSettlementDetailBeneficiaryName :: Maybe Text,
    rsfSettlementDetailStatus :: Maybe Text,
    rsfSettlementDetailReference :: Maybe Text,
    rsfSettlementDetailTimestamp :: Maybe UTCTimeRFC3339
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFSettlementDetail where
  parseJSON = genericParseJSON optionsRSFSettlementDetail

instance ToJSON RSFSettlementDetail where
  toJSON = genericToJSON optionsRSFSettlementDetail

optionsRSFSettlementDetail :: Options
optionsRSFSettlementDetail =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfSettlementDetailCounterparty", "settlement_counterparty"),
        ("rsfSettlementDetailPhase", "settlement_phase"),
        ("rsfSettlementDetailAmount", "settlement_amount"),
        ("rsfSettlementDetailType", "settlement_type"),
        ("rsfSettlementDetailBankAccountNo", "settlement_bank_account_no"),
        ("rsfSettlementDetailIfscCode", "settlement_ifsc_code"),
        ("rsfSettlementDetailUpiAddress", "upi_address"),
        ("rsfSettlementDetailBankName", "bank_name"),
        ("rsfSettlementDetailBranchName", "branch_name"),
        ("rsfSettlementDetailBeneficiaryAddress", "beneficiary_address"),
        ("rsfSettlementDetailBeneficiaryName", "beneficiary_name"),
        ("rsfSettlementDetailStatus", "settlement_status"),
        ("rsfSettlementDetailReference", "settlement_reference"),
        ("rsfSettlementDetailTimestamp", "settlement_timestamp")
      ]

data RSFPayerDetails = RSFPayerDetails
  { rsfPayerDetailsPayerName :: Maybe Text,
    rsfPayerDetailsPayerAddress :: Maybe Text,
    rsfPayerDetailsPayerAccountNo :: Maybe Text,
    rsfPayerDetailsPayerBankCode :: Maybe Text,
    rsfPayerDetailsPayerVirtualPaymentAddress :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFPayerDetails where
  parseJSON = genericParseJSON optionsRSFPayerDetails

instance ToJSON RSFPayerDetails where
  toJSON = genericToJSON optionsRSFPayerDetails

optionsRSFPayerDetails :: Options
optionsRSFPayerDetails =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfPayerDetailsPayerName", "payer_name"),
        ("rsfPayerDetailsPayerAddress", "payer_address"),
        ("rsfPayerDetailsPayerAccountNo", "payer_account_no"),
        ("rsfPayerDetailsPayerBankCode", "payer_bank_code"),
        ("rsfPayerDetailsPayerVirtualPaymentAddress", "payer_virtual_payment_address")
      ]

data RSFMonetaryValue = RSFMonetaryValue
  { rsfMonetaryValueCurrency :: Maybe Text,
    rsfMonetaryValueValue :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFMonetaryValue where
  parseJSON = genericParseJSON optionsRSFMonetaryValue

instance ToJSON RSFMonetaryValue where
  toJSON = genericToJSON optionsRSFMonetaryValue

optionsRSFMonetaryValue :: Options
optionsRSFMonetaryValue =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfMonetaryValueCurrency", "currency"),
        ("rsfMonetaryValueValue", "value")
      ]

data RSFProvider = RSFProvider
  { rsfProviderName :: Maybe RSFProviderName,
    rsfProviderAddress :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFProvider where
  parseJSON = genericParseJSON optionsRSFProvider

instance ToJSON RSFProvider where
  toJSON = genericToJSON optionsRSFProvider

optionsRSFProvider :: Options
optionsRSFProvider =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfProviderName", "name"),
        ("rsfProviderAddress", "address")
      ]

data RSFProviderName = RSFProviderName
  { rsfProviderNameName :: Maybe Text,
    rsfProviderNameCode :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFProviderName where
  parseJSON = genericParseJSON optionsRSFProviderName

instance ToJSON RSFProviderName where
  toJSON = genericToJSON optionsRSFProviderName

optionsRSFProviderName :: Options
optionsRSFProviderName =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfProviderNameName", "name"),
        ("rsfProviderNameCode", "code")
      ]

data RSFOnReceiverReconOrder = RSFOnReceiverReconOrder
  { rsfOnOrderId :: Maybe Text,
    rsfOnOrderInvoiceNo :: Maybe Text,
    rsfOnOrderCollectorAppId :: Maybe Text,
    rsfOnOrderReceiverAppId :: Maybe Text,
    rsfOnOrderOrderReconStatus :: Maybe Text,
    rsfOnOrderTransactionId :: Maybe Text,
    rsfOnOrderSettlementId :: Maybe Text,
    rsfOnOrderCounterpartyReconStatus :: Maybe Text,
    rsfOnOrderCounterpartyDiffAmount :: Maybe RSFCounterpartyDiffAmount,
    rsfOnOrderMessage :: Maybe RSFDiffMessage,
    rsfOnOrderSettlementDetails :: Maybe [RSFOnReconSettlementDetail]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFOnReceiverReconOrder where
  parseJSON = genericParseJSON optionsRSFOnOrder

instance ToJSON RSFOnReceiverReconOrder where
  toJSON = genericToJSON optionsRSFOnOrder

optionsRSFOnOrder :: Options
optionsRSFOnOrder =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfOnOrderId", "id"),
        ("rsfOnOrderInvoiceNo", "invoice_no"),
        ("rsfOnOrderCollectorAppId", "collector_app_id"),
        ("rsfOnOrderReceiverAppId", "receiver_app_id"),
        ("rsfOnOrderOrderReconStatus", "order_recon_status"),
        ("rsfOnOrderTransactionId", "transaction_id"),
        ("rsfOnOrderSettlementId", "settlement_id"),
        ("rsfOnOrderCounterpartyReconStatus", "counterparty_recon_status"),
        ("rsfOnOrderCounterpartyDiffAmount", "counterparty_diff_amount"),
        ("rsfOnOrderMessage", "message"),
        ("rsfOnOrderSettlementDetails", "settlement_details")
      ]

data RSFCounterpartyDiffAmount = RSFCounterpartyDiffAmount
  { rsfDiffAmountCurrency :: Maybe Text,
    rsfDiffAmountValue :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFCounterpartyDiffAmount where
  parseJSON = genericParseJSON optionsRSFDiffAmount

instance ToJSON RSFCounterpartyDiffAmount where
  toJSON = genericToJSON optionsRSFDiffAmount

optionsRSFDiffAmount :: Options
optionsRSFDiffAmount =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfDiffAmountCurrency", "currency"),
        ("rsfDiffAmountValue", "value")
      ]

data RSFDiffMessage = RSFDiffMessage
  { rsfDiffMessageName :: Maybe Text,
    rsfDiffMessageCode :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFDiffMessage where
  parseJSON = genericParseJSON optionsRSFDiffMessage

instance ToJSON RSFDiffMessage where
  toJSON = genericToJSON optionsRSFDiffMessage

optionsRSFDiffMessage :: Options
optionsRSFDiffMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfDiffMessageName", "name"),
        ("rsfDiffMessageCode", "code")
      ]

newtype RSFAck = RSFAck
  { rsfAckStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFAck where
  parseJSON = genericParseJSON optionsRSFAck

instance ToJSON RSFAck where
  toJSON = genericToJSON optionsRSFAck

optionsRSFAck :: Options
optionsRSFAck =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table = [("rsfAckStatus", "status")]

newtype RSFAckMessage = RSFAckMessage
  { rsfAckMessageAck :: RSFAck
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFAckMessage where
  parseJSON = genericParseJSON optionsRSFAckMessage

instance ToJSON RSFAckMessage where
  toJSON = genericToJSON optionsRSFAckMessage

optionsRSFAckMessage :: Options
optionsRSFAckMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table = [("rsfAckMessageAck", "ack")]

data RSFAckResponse = RSFAckResponse
  { rsfAckResponseError :: Maybe RSFError,
    rsfAckResponseMessage :: RSFAckMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFAckResponse where
  parseJSON = genericParseJSON optionsRSFAckResponse

instance ToJSON RSFAckResponse where
  toJSON = genericToJSON optionsRSFAckResponse

optionsRSFAckResponse :: Options
optionsRSFAckResponse =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfAckResponseError", "error"),
        ("rsfAckResponseMessage", "message")
      ]

data RSFError = RSFError
  { rsfErrorMessage :: Maybe Text,
    rsfErrorCode :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFError where
  parseJSON = genericParseJSON optionsRSFError

instance ToJSON RSFError where
  toJSON = genericToJSON optionsRSFError

optionsRSFError :: Options
optionsRSFError =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfErrorMessage", "message"),
        ("rsfErrorCode", "code")
      ]

newtype RSFOnReceiverReconMessage = RSFOnReceiverReconMessage
  { rsfOnReceiverReconMessageOrderbook :: RSFOnReceiverReconOrderbook
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFOnReceiverReconMessage where
  parseJSON = genericParseJSON optionsRSFOnReceiverReconMessage

instance ToJSON RSFOnReceiverReconMessage where
  toJSON = genericToJSON optionsRSFOnReceiverReconMessage

optionsRSFOnReceiverReconMessage :: Options
optionsRSFOnReceiverReconMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table = [("rsfOnReceiverReconMessageOrderbook", "orderbook")]

newtype RSFOnReceiverReconOrderbook = RSFOnReceiverReconOrderbook
  { rsfOnReceiverReconOrderbookOrders :: [RSFOnReceiverReconOrder]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFOnReceiverReconOrderbook where
  parseJSON = genericParseJSON optionsRSFOnReceiverReconOrderbook

instance ToJSON RSFOnReceiverReconOrderbook where
  toJSON = genericToJSON optionsRSFOnReceiverReconOrderbook

optionsRSFOnReceiverReconOrderbook :: Options
optionsRSFOnReceiverReconOrderbook =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table = [("rsfOnReceiverReconOrderbookOrders", "orders")]

data RSFOnReconSettlementDetail = RSFOnReconSettlementDetail
  { rsfOnReconSdSettlementId :: Maybe Text,
    rsfOnReconSdSettlementReferenceNo :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RSFOnReconSettlementDetail where
  parseJSON = genericParseJSON optionsRSFOnReconSettlementDetail

instance ToJSON RSFOnReconSettlementDetail where
  toJSON = genericToJSON optionsRSFOnReconSettlementDetail

optionsRSFOnReconSettlementDetail :: Options
optionsRSFOnReconSettlementDetail =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("rsfOnReconSdSettlementId", "settlement_id"),
        ("rsfOnReconSdSettlementReferenceNo", "settlement_reference_no")
      ]
