module Core.Spec.Common.Payment (module Core.Spec.Common.Payment, module Decimal) where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow, Showable (..))
import Beckn.Utils.JSON
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Common.DecimalValue as Decimal (DecimalValue (..))
import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi
  ( ToSchema (declareNamedSchema),
    fromAesonOptions,
    genericDeclareNamedSchema,
  )

data Payment a = Payment
  { uri :: BaseUrl,
    tl_method :: TLMethod,
    params :: a,
    _type :: PaymentType,
    status :: Status
  }
  deriving (Generic, Show)

instance (ToSchema a) => ToSchema (Payment a) where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance (FromJSON a) => FromJSON (Payment a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance (ToJSON a) => ToJSON (Payment a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

----------------------------------------------

data TLMethod = HttpGet | HttpPost
  deriving (Show, Generic)
  deriving anyclass (ToSchema)
  deriving (PrettyShow) via Showable TLMethod

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
  deriving (PrettyShow) via Showable PaymentType

data Status = PAID | NOT_PAID
  deriving (Generic, Eq, Show)
  deriving (PrettyShow) via Showable Status

instance ToSchema PaymentType where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance ToSchema Status where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

instance FromJSON Status where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON Status where
  toJSON = genericToJSON constructorsWithHyphens

data TrStatus
  = CAPTURED
  | FAILED
  | PAYMENT_LINK_CREATED
  | PAYMENT_LINK_EXPIRED
  | PAYMENT_LINK_ISSUED
  | REFUNDED
  deriving (Generic, Show, Eq)
  deriving (PrettyShow) via Showable TrStatus

instance ToSchema TrStatus where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsToLowerOptions

instance FromJSON TrStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON TrStatus where
  toJSON = genericToJSON constructorsToLowerOptions
