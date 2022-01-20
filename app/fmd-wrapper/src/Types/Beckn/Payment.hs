module Types.Beckn.Payment
  ( Payment (..),
    PaymentType (..),
    Params (..),
  )
where

import Beckn.Utils.JSON
import EulerHS.Prelude hiding (State, (.=))
import Types.Beckn.DecimalValue (DecimalValue)

data Payment = Payment
  { params :: Params,
    _type :: PaymentType
  }
  deriving (Generic, Show)

data Params = Params
  { amount :: Maybe DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data PaymentType
  = POST_FULFILLMENT
  deriving (Generic, Eq, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens
