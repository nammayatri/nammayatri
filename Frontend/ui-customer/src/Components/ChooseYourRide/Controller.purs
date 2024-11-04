module Components.ChooseYourRide.Controller where

import Data.Maybe (Maybe(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.PrimaryButton.Controller as PrimaryButtonController
import ConfigProvider
import MerchantConfig.Types
import Screens.Types(TipViewProps, TipViewStage(..), ZoneType(..), FareProductType(..))
import Prelude (negate)
import Resources.Constants (intMin, intMax)

data Action
  = NoAction Config
  | ChooseVehicleAC TipViewProps ChooseVehicleController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action
  | PreferencesDropDown
  | RadioButtonClick Boolean
  | OnIconClick Boolean
  | SpecialZoneInfoTag
  | TipBtnClick Int Int (Array Int)
  | AddTip TipViewProps
  | ChangeTip TipViewProps


type Config
  = { rideDistance :: String
    , rideDuration :: String
    , rideTime :: String
    , activeIndex :: Int
    , quoteList :: Array ChooseVehicleController.Config
    , nearByDrivers :: Maybe Int
    , showPreferences :: Boolean
    , bookingPreferenceEnabled :: Boolean
    , flowWithoutOffers :: Boolean
    , selectedEstimateHeight :: Int
    , zoneType :: ZoneType
    , tipViewProps :: TipViewProps
    , tipForDriver :: Int
    , customerTipArray :: Array String
    , customerTipArrayWithValues :: Array Int
    , enableTips :: Boolean
    , showMultiProvider :: Boolean
    , currentEstimateHeight :: Int
    , intercity :: Boolean
    , fareProductType :: FareProductType 
    , startTimeUTC :: Maybe String
    , returnTimeUTC :: Maybe String
    , roundTrip :: Boolean
    }

type BookAnyProps 
  = { minPrice :: Int
    , maxPrice :: Int
    , minCapacity :: Int
    , maxCapacity :: Int 
    }

config :: Config
config =
  { rideDistance: ""
  , rideDuration: ""
  , rideTime: ""
  , activeIndex: 0
  , quoteList: []
  , nearByDrivers : Nothing
  , showPreferences : false
  , bookingPreferenceEnabled : false
  , flowWithoutOffers : false
  , selectedEstimateHeight : 0
  , zoneType : NOZONE
  , customerTipArray : []
  , customerTipArrayWithValues : []
  , currentEstimateHeight : 0
  , tipViewProps : {
      stage : DEFAULT
    , isVisible : false
    , onlyPrimaryText : false
    , isprimaryButtonVisible : false
    , primaryText : ""
    , secondaryText : ""
    , customerTipArray : []
    , customerTipArrayWithValues : []
    , activeIndex : -1
    , primaryButtonText : ""
    }
  , tipForDriver : 0
  , enableTips : true
  , showMultiProvider : false
  , intercity : false
  , fareProductType : ONE_WAY
  , startTimeUTC : Nothing
  , returnTimeUTC : Nothing
  , roundTrip : false
  }

bookAnyProps :: BookAnyProps
bookAnyProps = 
  { minPrice : intMax
  , maxPrice : intMin
  , minCapacity : intMax
  , maxCapacity : intMin
  }