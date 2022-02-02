module Core.Confirm where

import Core.Billing
import Core.Payment
import Core.Provider
import Core.Quotation
import Data.Aeson
import Relude hiding (id)
import Core.Confirm.Item
import Beckn.Types.Amount
import Beckn.Types.Core.Migration.DecimalValue

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
