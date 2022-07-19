module Beckn.Types.Core.Taxi.OnInit.Quote where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.DecimalValue
import Beckn.Types.Core.Taxi.OnInit.BreakupItem
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

data Quote = Quote
  { price :: QuotePrice,
    breakup :: [BreakupItem]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Quote where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data QuotePrice = QuotePrice
  { currency :: Text,
    value :: DecimalValue,
    offered_value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema QuotePrice where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
