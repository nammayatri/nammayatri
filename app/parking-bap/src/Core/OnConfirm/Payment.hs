module Core.OnConfirm.Payment where

import Beckn.Prelude
import Beckn.Utils.JSON

data PaymentStatus = PAID | NOT_PAID deriving (Generic, Eq)

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentType = PRE_FULFILLMENT | POST_FULFILLMENT | ON_ORDER deriving (Generic)

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentGatewayTransactionStatus = PAYMENT_LINK_CREATED | PAYMENT_LINK_EXPIRED | CAPTURED | REFUNDED deriving (Generic, Show)

instance FromJSON PaymentGatewayTransactionStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON PaymentGatewayTransactionStatus where
  toJSON = genericToJSON constructorsToLowerOptions

data PaymentParams = PaymentParams
  { amount :: Text,
    currency :: Text,
    transaction_status :: PaymentGatewayTransactionStatus,
    transaction_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data Payment = Payment
  { params :: PaymentParams,
    _type :: PaymentType,
    status :: PaymentStatus,
    uri :: BaseUrl,
    tl_method :: Text
  }
  deriving (Generic)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
