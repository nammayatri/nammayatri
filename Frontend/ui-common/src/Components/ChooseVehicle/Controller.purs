module Components.ChooseVehicle.Controller where

import Prelude (class Eq, class Show )
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
    , price :: String
    , isCheckBox :: Boolean
    , isEnabled :: Boolean
    , index :: Int
    , activeIndex :: Int
    , id :: String
    , maxPrice :: Int
    , basePrice :: Int
    , showInfo :: Boolean
    , searchResultType :: SearchType
    , isBookingOption :: Boolean
    }

data SearchType = QUOTES | ESTIMATES

derive instance genericSearchType :: Generic SearchType _
instance eqSearchType :: Eq SearchType where eq = genericEq
instance showSearchType :: Show SearchType where show = genericShow


config :: Config
config =
  { vehicleImage: ""
  , isSelected: false
  , vehicleVariant: ""
  , vehicleType: ""
  , capacity: ""
  , price: ""
  , isCheckBox: false
  , isEnabled: true
  , activeIndex: 0
  , index: 0
  , id: ""
  , maxPrice : 123
  , basePrice : 0 
  , showInfo : false
  , searchResultType : QUOTES
  , isBookingOption : false
  }
