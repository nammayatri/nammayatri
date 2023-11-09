module Components.ChooseVehicle.Controller where

import Prelude (class Eq, class Show )
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (decode, encode, class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)

data Action
  = NoAction
  | OnSelect Config
  | OnImageClick
  | ShowRateCard String
  | ChangeTicketQuantity Boolean
  | ShowRouteInfo

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
    , quantity :: Int
    }

data SearchType = QUOTES | ESTIMATES

derive instance genericSearchType :: Generic SearchType _
instance eqSearchType :: Eq SearchType where eq = genericEq
instance showSearchType :: Show SearchType where show = genericShow
instance encodeSearchType :: Encode SearchType where encode = defaultEnumEncode
instance decodeSearchType :: Decode SearchType where decode = defaultEnumDecode


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
  , quantity : 0
  }

dummyBusConfig :: Config 
dummyBusConfig =
  { vehicleImage: "ny_ic_bus"
  , isSelected: true
  , vehicleVariant: "BUS"
  , vehicleType: "Local Bus"
  , capacity: "1 passenger"
  , price: "₹20"
  , isCheckBox: false
  , isEnabled: true
  , activeIndex: 0
  , index: 0
  , id: "123456"
  , maxPrice : 123
  , basePrice : 20
  , showInfo : true
  , searchResultType : ESTIMATES
  , isBookingOption : true
  , quantity : 1
  }