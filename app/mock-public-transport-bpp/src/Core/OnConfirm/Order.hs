module Core.OnConfirm.Order where

import Beckn.Prelude
import Core.Common.Billing
import Core.Common.OrderState
import Core.Common.Payment
import Core.Common.ProviderId
import Core.Common.Quotation
import Core.OnConfirm.Item
import Core.OnConfirm.Params

data Order = Order
  { id :: Text,
    state :: State,
    provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON)
