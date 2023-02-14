 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Spec.Common.Payment (module Beckn.Spec.Common.Payment, module Decimal) where

import Beckn.Spec.Common.DecimalValue as Decimal (DecimalValue (..))
import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi
  ( ToSchema (declareNamedSchema),
    fromAesonOptions,
    genericDeclareNamedSchema,
  )
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow, Showable (..))
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

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
