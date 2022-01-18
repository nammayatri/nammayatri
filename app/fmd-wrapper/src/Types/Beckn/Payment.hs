module Types.Beckn.Payment
  ( Payment (..),
    PaymentType (..),
    Params (..),
  )
where

import Beckn.Utils.JSON
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi
  ( ToSchema (..),
    defaultSchemaOptions,
    fromAesonOptions,
    genericDeclareNamedSchema,
  )
import EulerHS.Prelude hiding (State, (.=))
import Types.Beckn.DecimalValue (DecimalValue)

data Payment = Payment
  { params :: Params,
    _type :: PaymentType
  }
  deriving (Generic, Show)

instance ToSchema Payment where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Params = Params
  { amount :: Maybe DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance ToSchema Params where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data PaymentType
  = POST_FULFILLMENT
  deriving (Generic, Eq, Show)

instance ToSchema PaymentType where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens
