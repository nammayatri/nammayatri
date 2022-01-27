module Core.Spec.Common.Price where

import Beckn.Types.Amount
import Beckn.Types.Core.Migration.DecimalValue
import Beckn.Utils.GenericPretty
import Data.Aeson
import Data.OpenApi (ToSchema)
import Relude

data Price = Price
  { currency :: Text,
    value :: Amount
  }
  deriving (Generic, Show, ToSchema, PrettyShow)

instance FromJSON Price where
  parseJSON = withObject "price" $ \obj -> do
    currency <- obj .: "currency"
    decimalValue <- obj .: "value"
    value <- maybe (fail "invalid price value") pure $ convertDecimalValueToAmount decimalValue
    pure Price {..}

instance ToJSON Price where
  toJSON p =
    object
      [ "currency" .= p.currency,
        "value" .= convertAmountToDecimalValue (p.value)
      ]

rupeePrice :: Amount -> Price
rupeePrice = Price "INR"
