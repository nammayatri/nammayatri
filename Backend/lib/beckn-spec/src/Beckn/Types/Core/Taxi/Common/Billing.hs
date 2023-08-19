module Beckn.Types.Core.Taxi.Common.Billing where

import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Billing = Billing
  { name :: Maybe Text,
    phone :: Maybe Text
  }
  deriving (Generic, Show)

instance ToSchema Billing where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Billing where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON Billing where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
