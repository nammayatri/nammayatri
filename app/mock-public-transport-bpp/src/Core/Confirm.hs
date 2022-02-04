module Core.Confirm where

import Beckn.Types.Amount
import Beckn.Types.Core.Migration.DecimalValue
import Core.Common.Billing
import Core.Common.Payment
import Core.Common.ProviderId
import Core.Common.Quotation
import Core.Confirm.Item
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
    --    fulfillment :: FulfillmentId,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Params = Params
  --  { amount :: DecimalValue,
  { amount :: Amount,
    currency :: Text
  }
  deriving (Generic, Eq, Show)

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
