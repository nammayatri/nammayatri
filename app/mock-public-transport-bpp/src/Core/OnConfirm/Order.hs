module Core.OnConfirm.Order where

import Beckn.Prelude
import Core.Common.Billing
import Core.OnConfirm.Fulfillment
import Core.OnConfirm.Item
import Core.OnConfirm.Params
import Core.Common.OrderState
import Core.Common.Payment
import Core.Common.Quotation
import Core.Common.ProviderId

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
