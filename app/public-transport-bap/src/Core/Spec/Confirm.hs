module Core.Spec.Confirm where

import Beckn.Types.Amount
import Beckn.Types.Core.Migration.DecimalValue
import Core.Spec.Common.Billing
import Core.Spec.Common.Payment
import Core.Spec.Common.ProviderId
import Core.Spec.Common.Quotation
import Core.Spec.Confirm.Item
import Data.Aeson
import Relude hiding (id)

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Params = Params
  { currency :: Text,
    amount :: Amount
  }
  deriving (Generic, Eq, Show)

rupeeParams :: Amount -> Params
rupeeParams = Params "INR"

instance FromJSON Params where
  parseJSON = withObject "params" $ \obj -> do
    currency <- obj .: "currency"
    decimalValue <- obj .: "amount"
    amount <- maybe (fail "invalid params value") pure $ convertDecimalValueToAmount decimalValue
    pure Params {..}

instance ToJSON Params where
  toJSON p =
    object
      [ "currency" .= p.currency,
        "amount" .= convertAmountToDecimalValue (p.amount)
      ]
