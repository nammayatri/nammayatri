module Core.Spec.Common.Price where

import Core.Spec.Common.DecimalValue
import Kernel.Prelude
import Kernel.Utils.GenericPretty

data Price = Price
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, PrettyShow)

rupeePrice :: DecimalValue -> Price
rupeePrice = Price "INR"
