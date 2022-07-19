module Beckn.Types.Core.Metro.Search.Price (Price (..)) where

import Beckn.Types.Core.Metro.Common.DecimalValue (DecimalValue)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Price = Price
  { currency :: Maybe Text,
    value :: Maybe DecimalValue,
    estimated_value :: Maybe DecimalValue,
    computed_value :: Maybe DecimalValue,
    listed_value :: Maybe DecimalValue,
    offered_value :: Maybe DecimalValue,
    minimum_value :: Maybe DecimalValue,
    maximum_value :: Maybe DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
