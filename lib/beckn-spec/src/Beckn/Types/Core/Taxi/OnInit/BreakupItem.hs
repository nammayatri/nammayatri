module Beckn.Types.Core.Taxi.OnInit.BreakupItem where

import Beckn.Types.Core.Taxi.Common.DecimalValue
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data BreakupItem = BreakupItem
  { title :: Text,
    price :: BreakupItemPrice
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema BreakupItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BreakupItemPrice = BreakupItemPrice
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema BreakupItemPrice where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
