module Core.Price where

import Beckn.Types.Core.Migration.DecimalValue
import Data.Aeson
import Data.OpenApi (ToSchema)
import Relude
import Beckn.Types.Amount

data Price = Price
  { currency :: Text,
--    value :: DecimalValue
    value :: Amount
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Price where
  parseJSON = withObject "price" $ \obj -> do
    currency <- obj .: "currency"
    decimalValue <- obj .: "value"
    value <- maybe (fail "invalid price value") pure $ convertDecimalValueToAmount decimalValue
    pure Price {..}

instance ToJSON Price where
  toJSON p = object [
    "currency" .= p.currency,
    "value" .= convertAmountToDecimalValue (p.value)
    ]
