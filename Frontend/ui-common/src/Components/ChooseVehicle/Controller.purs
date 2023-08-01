module Components.ChooseVehicle.Controller where

data Action
  = NoAction
  | OnSelect Config
  | OnImageClick
  | ShowRateCard String

type Config
  = { vehicleImage :: String
    , isSelected :: Boolean
    , vehicleVariant :: String
    , vehicleType :: String
    , capacity :: String
    , price :: Int
    , isCheckBox :: Boolean
    , isEnabled :: Boolean
    , index :: Int
    , activeIndex :: Int
    , id :: String
    , maxPrice :: Int
    , basePrice :: Int
    }

config :: Config
config =
  { vehicleImage: ""
  , isSelected: false
  , vehicleVariant: "Non AC Taxi"
  , vehicleType: "Economical"
  , capacity: "4 people"
  , price: 246
  , isCheckBox: false
  , isEnabled: true
  , activeIndex: 0
  , index: 0
  , id: ""
  , maxPrice : 123
  , basePrice : 0 
  }
