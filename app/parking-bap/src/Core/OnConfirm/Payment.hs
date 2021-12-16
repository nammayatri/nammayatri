module Core.OnConfirm.Payment where

import Beckn.Prelude
import Beckn.Utils.JSON

data PaymentStatus = PAID | NOT_PAID deriving (Generic, Eq) --TODO: WHAT ARE POSSIBLE VALUES?

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

data PaymentType = PRE_FULFILLMENT | POST_FULFILLMENT deriving (Generic) --TODO: WHAT ARE POSSIBLE VALUES?

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

data SpecTransactionStatus = PAYMENT_LINK_ISSUED | PAYMENT_LINK_NOT_ISSUED | CAPTURED deriving (Generic) --TODO: WHAT ARE POSSIBLE VALUES?

instance FromJSON SpecTransactionStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

data PaymentParams = PaymentParams
  { amount :: Int,
    currency :: Text,
    transaction_status :: SpecTransactionStatus,
    transaction_id :: Text
  }
  deriving (Generic, FromJSON)

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
