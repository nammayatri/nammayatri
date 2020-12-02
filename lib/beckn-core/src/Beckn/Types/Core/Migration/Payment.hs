{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Payment
  ( Payment (..),
    PaymentType (..),
    TLMethod (..),
  )
where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON (constructorsWithHyphens)
import Data.Aeson (Value (..))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import EulerHS.Prelude hiding (State, (.=))
import Servant.Client (BaseUrl)

data Payment = Payment
  { _uri :: Maybe BaseUrl,
    _tl_method :: Maybe TLMethod,
    _params :: Maybe Params,
    _type :: Maybe PaymentType,
    _status :: Maybe Status,
    _time :: Maybe Time
  }
  deriving (Generic, Show)

data TLMethod = HttpGet | HttpPost
  deriving (Show)

instance FromJSON TLMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "http/post") = pure HttpPost
  parseJSON e = typeMismatch "tl_method string" e

instance ToJSON TLMethod where
  toJSON HttpGet = String "http/get"
  toJSON HttpPost = String "http/post"

data Params = Params
  { _transaction_id :: Maybe Text,
    _amount :: Maybe DecimalValue
    -- _amount :: Maybe MonetaryValue -- DELETEME: should it be like this?
    -- _additional :: HashMap Text Text -- DELETEME
  }
  deriving (Generic, Show)

{- DELETEME:
   possible implementation of Params to support
     additionalProperties: { "type": "string" }

instance FromJSON Params where
  parseJSON = withObject "Params" $ \o -> Params
    <$> o .: "transaction_id"
    <*> o .: "amount"
    <*> pure o -- error

instance ToJSON Params where
  toJSON Params {..} = uniteObjects [knownParams, Object _additional]
    where knownParams = object
            [ "transaction_id" .= _transaction_id,
              "amount" .= _amount
            ]
-}

data PaymentType
  = ON_ORDER
  | PRE_FULFILLMENT
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show)

data Status = PAID | NOT_PAID
  deriving (Generic, Eq, Show)

deriveJSON stripAllLensPrefixOptions ''Payment
deriveJSON constructorsWithHyphens ''PaymentType
deriveJSON constructorsWithHyphens ''Status
deriveJSON stripAllLensPrefixOptions ''Params
