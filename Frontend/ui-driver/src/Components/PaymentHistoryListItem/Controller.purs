module Components.PaymentHistoryListItem.Controller where

import Services.APITypes (PaymentStatus)

data Action
  = OnClick String

type Config
  = { isSelected :: Boolean
    , charges :: Number
    , totalEarning :: Number
    , totalRides :: Int
    , date :: String
    , status :: PaymentStatus
    , paymentBreakUp :: Array PaymnetBreakUp
    , id :: String
    }
type PaymnetBreakUp = {
  description :: String
, amount :: Number
}
