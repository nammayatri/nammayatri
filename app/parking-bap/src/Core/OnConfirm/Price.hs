module Core.OnConfirm.Price where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)

data Price = Price
  { currency :: Text,
    value :: Maybe DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)