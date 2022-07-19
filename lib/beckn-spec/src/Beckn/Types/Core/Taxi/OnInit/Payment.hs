module Beckn.Types.Core.Taxi.OnInit.Payment
  ( module Beckn.Types.Core.Taxi.OnInit.Payment,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentType as Reexport
import Beckn.Types.Core.Taxi.Common.TimeDuration as Reexport
import Beckn.Utils.JSON as JSON
import Beckn.Utils.Schema
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)

data Payment = Payment
  { collected_by :: Text,
    params :: PaymentParams,
    _type :: PaymentType,
    time :: TimeDuration
  }
  deriving (Generic, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON JSON.stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON JSON.stripPrefixUnderscoreIfAny

instance ToSchema Payment where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions JSON.stripPrefixUnderscoreIfAny

data PaymentParams = PaymentParams
  { currency :: Text,
    amount :: DecimalValue
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema PaymentParams where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
