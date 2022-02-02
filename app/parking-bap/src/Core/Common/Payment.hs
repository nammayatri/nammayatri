module Core.Common.Payment where

import Beckn.Prelude
import Beckn.Types.App ()
import Beckn.Utils.JSON
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), fromAesonOptions, genericDeclareNamedSchema)

data PaymentStatus = PAID | NOT_PAID deriving (Generic, Eq)

instance ToSchema PaymentStatus where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentType = PRE_FULFILLMENT | POST_FULFILLMENT | ON_ORDER deriving (Generic)

instance ToSchema PaymentType where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentGatewayTransactionStatus = PAYMENT_LINK_CREATED | PAYMENT_LINK_EXPIRED | CAPTURED | REFUNDED deriving (Generic, Show)

instance ToSchema PaymentGatewayTransactionStatus where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsToLowerOptions

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

instance ToSchema PaymentParams where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

data Payment = Payment
  { params :: PaymentParams,
    _type :: PaymentType,
    status :: PaymentStatus,
    uri :: BaseUrl,
    tl_method :: Text
  }
  deriving (Generic)

instance ToSchema Payment where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
