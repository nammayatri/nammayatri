{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.Search.Payment
  ( Payment (..),
    PaymentType (..),
    TLMethod (..),
    Params (..),
  )
where

import Beckn.Types.Core.Metro.Search.Time (Time)
import Data.Aeson (Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.KeyMap as AKM (delete, fromHashMapText, toHashMapText)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State)
import Kernel.Types.App (BaseUrl)
import Kernel.Types.Beckn.DecimalValue (DecimalValue)
import Kernel.Utils.Example
import Kernel.Utils.JSON

data Payment = Payment
  { uri :: Maybe BaseUrl,
    tl_method :: Maybe TLMethod,
    params :: Maybe Params,
    _type :: Maybe PaymentType,
    status :: Maybe Status,
    time :: Maybe Time
  }
  deriving (Generic, Show, ToSchema)

data TLMethod = HttpGet | HttpPost
  deriving (Generic, Show, ToSchema)

instance FromJSON TLMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "http/post") = pure HttpPost
  parseJSON e = typeMismatch "tl_method string" e

instance ToJSON TLMethod where
  toJSON HttpGet = String "http/get"
  toJSON HttpPost = String "http/post"

data Params = Params
  { transaction_id :: Maybe Text,
    transaction_status :: Maybe Text,
    amount :: Maybe DecimalValue,
    currency :: Text,
    additional :: HashMap Text Text
  }
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON Params where
  parseJSON = withObject "Params" $ \o ->
    Params
      <$> o .: "transaction_id"
      <*> o .: "transaction_status"
      <*> o .: "amount"
      <*> o .: "currency"
      <*> mapM f (AKM.toHashMapText $ additional o)
    where
      f (String val) = pure val
      f e = typeMismatch "additional property of Params" e
      additional =
        AKM.delete "transaction_id"
          . AKM.delete "transaction_status"
          . AKM.delete "amount"
          . AKM.delete "currency"

instance ToJSON Params where
  toJSON Params {..} = uniteObjects [object knownParams, Object $ AKM.fromHashMapText (String <$> additional)]
    where
      knownParams =
        [ "transaction_id" .= transaction_id,
          "transaction_status" .= transaction_status,
          "amount" .= amount,
          "currency" .= currency
        ]

data BankAccount = BankAccount
  { ifsc_code :: Maybe Text,
    account_number :: Maybe Text,
    account_holder_name :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

data PaymentType
  = ON_ORDER
  | PRE_FULFILLMENT
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show, ToSchema)

data Status = PAID | NOT_PAID
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Payment where
  example =
    Payment
      { uri = Nothing,
        tl_method = Nothing,
        params = Nothing,
        _type = Nothing,
        status = Nothing,
        time = Nothing
      }

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Status where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON Status where
  toJSON = genericToJSON constructorsWithHyphens
