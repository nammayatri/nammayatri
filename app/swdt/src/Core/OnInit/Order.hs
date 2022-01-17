module Core.OnInit.Order where

import Beckn.Prelude
import Core.Billing
import Core.OnInit.Item
import Core.OnInit.Payment
import Core.OnInit.Quotation
import Core.OnSearch.Fulfillment
import Core.Provider

newtype OnInitMessage = OnInitMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    fulfillment :: Fulfillment,
    quote :: Quotation,
    payment :: Payment
  }
  deriving (Generic, Show, ToJSON, FromJSON)
