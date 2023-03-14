module Components.RateCard.Controller where

data Action = Close | BackPressed | NoAction

type Config = {
    baseFare :: String,
    extraFare :: String,
    pickUpCharges :: String,
    additionalFare :: String,
    nightCharges :: Boolean,
    nightShiftMultiplier :: String 
}

config :: Config 
config = {
    baseFare : "₹45",
    extraFare : "₹23",
    pickUpCharges : "₹10", 
    additionalFare : "₹30",
    nightCharges : false,
    nightShiftMultiplier : "1.5"
}
