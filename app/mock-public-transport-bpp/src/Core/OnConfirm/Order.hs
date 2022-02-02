module Core.OnConfirm.Order where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.OnConfirm.Item
import Core.OnConfirm.Params
import Core.OrderState
import Core.Payment
import Core.Provider
import Core.Quotation

data Order = Order
  { id :: Text,
    state :: State,
    provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    fulfillment :: Fulfillment,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON)
