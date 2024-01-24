module Components.PaymentHistoryListItem.Controller where

import Domain.Payments (PaymentStatus)

data Action
  = OnClick String

type Config
  = { isSelected :: Boolean
    , charges :: Int
    , totalEarning :: Int
    , totalRides :: Int
    , date :: String
    , status :: PaymentStatus
    , paymentBreakUp :: Array PaymentBreakUp
    , id :: String
    }
type PaymentBreakUp = {
  description :: String
, amount :: Number
}
