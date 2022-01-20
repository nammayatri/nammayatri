module Core.Payment (module Core.Payment, module Decimal) where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue as Decimal (DecimalValue (..))
import Beckn.Utils.JSON
import Data.Aeson
import Data.Aeson.Types

data Payment a = Payment
  { uri :: BaseUrl,
    tl_method :: TLMethod,
    params :: a,
    _type :: PaymentType,
    status :: Status
  }
  deriving (Generic, Show)

instance (FromJSON a) => FromJSON (Payment a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance (ToJSON a) => ToJSON (Payment a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

----------------------------------------------

data TLMethod = HttpGet | HttpPost
  deriving (Show)

instance FromJSON TLMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "http/post") = pure HttpPost
  parseJSON e = typeMismatch "tl_method string" e

instance ToJSON TLMethod where
  toJSON HttpGet = String "http/get"
  toJSON HttpPost = String "http/post"

data PaymentType
  = ON_ORDER
  | PRE_FULFILLMENT
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show)

data Status = PAID | NOT_PAID
  deriving (Generic, Eq, Show)

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Status where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON Status where
  toJSON = genericToJSON constructorsWithHyphens

data TrStatus
  = Captured
  | Failed
  | PaymentLinkCreated
  | PaymentLinkExpired
  | PaymentLinkIssued
  | Refunded
  deriving (Generic, Eq, Show, ToJSON, FromJSON)
