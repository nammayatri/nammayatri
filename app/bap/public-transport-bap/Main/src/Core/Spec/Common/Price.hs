module Core.Spec.Common.Price where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Core.Spec.Common.DecimalValue

data Price = Price
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, PrettyShow)

rupeePrice :: DecimalValue -> Price
rupeePrice = Price "INR"
