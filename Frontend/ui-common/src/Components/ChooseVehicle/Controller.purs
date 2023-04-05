module Components.ChooseVehicle.Controller where

data Action
  = NoAction
  | OnSelect Config
  | OnImageClick

type Config
  = { vehicleImage :: String
    , isSelected :: Boolean
    , vehicleVariant :: String
    , vehicleType :: String
    , capacity :: String
    , price :: String
    , isCheckBox :: Boolean
    , isEnabled :: Boolean
    , index :: Int
    , activeIndex :: Int
    , id :: String
    }

config :: Config
config =
  { vehicleImage: "ny_ic_Sedan_Yellow.png,https://assets.juspay.in/nammayatri/images/user/ny_ic_sedan_yellow.png"
  , isSelected: false
  , vehicleVariant: "Non AC Taxi"
  , vehicleType: "Economical"
  , capacity: "4 people"
  , price: "246"
  , isCheckBox: false
  , isEnabled: true
  , activeIndex : 0
  , index : 0
  , id : ""
  }
