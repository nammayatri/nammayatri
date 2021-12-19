module Core.Order where

import Beckn.Prelude
import Core.OnConfirm.Billing
import Core.OnConfirm.Fulfillment
import Core.OnConfirm.Item
import Core.OnConfirm.Payment
import Core.OnConfirm.Provider
import Core.OnConfirm.SpecQuote

data OrderState = ACTIVE | CANCELLED | COMPLETE
  deriving (Generic, FromJSON, ToJSON)

newtype OrderProviderLocation = OrderProviderLocation
  { id :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Order = Order
  { id :: Text,
    state :: Maybe OrderState,
    provider :: Provider,
    provider_location :: OrderProviderLocation,
    items :: [Item],
    billing :: Billing,
    fulfillment :: Fulfillment,
    quote :: SpecQuote,
    payment :: Payment,
    created_at :: UTCTime,
    updated_at :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)
