module Components.ChooseVehicle.Controller where

import Prelude (class Eq, class Show )
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (decode, encode, class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import PrestoDOM (Margin(..))
import Data.Maybe (Maybe(..))
import Common.Types.App (RateCardType(..), FareList)

data Action
  = NoAction
  | OnSelect Config
  | OnImageClick
  | ShowRateCard Config
  | OnEditClick

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
    , pickUpCharges :: Int 
    , layoutMargin :: Margin
    , isSingleEstimate :: Boolean
    , tollCharge :: Int
    , serviceTierShortDesc :: Maybe String
    , serviceTierName :: Maybe String
    , extraFare :: Array FareList
    , additionalFare :: Int
    , nightShiftMultiplier :: Number
    , nightCharges :: Boolean
    , baseFare :: Int
    , airConditioned :: Maybe Boolean
    , showEditButton :: Boolean
    , editBtnText :: String
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
  , pickUpCharges : 0
  , layoutMargin : MarginHorizontal 12 12
  , isSingleEstimate : false
  , tollCharge : 0
  , serviceTierShortDesc : Nothing
  , serviceTierName : Nothing
  , extraFare: []
  , additionalFare: 0
  , nightShiftMultiplier: 0.0
  , nightCharges: false
  , baseFare : 0
  , airConditioned : Nothing
  , showEditButton : false
  , editBtnText : ""
  }