module Components.ChooseYourRide.Controller where

import Data.Maybe (Maybe(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.PrimaryButton.Controller as PrimaryButtonController
import ConfigProvider
import MerchantConfig.Types
import Screens.Types(TipViewProps, TipViewStage(..), ZoneType(..))
import Prelude (negate)

data Action
  = NoAction
  | ChooseVehicleAC ChooseVehicleController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action
  | PreferencesDropDown
  | RadioButtonClick Boolean
  | OnIconClick Boolean
  | SpecialZoneInfoTag
  | TipBtnClick Int Int
  | AddTip
  | ChangeTip


type Config
  = { rideDistance :: String
    , rideDuration :: String
    , activeIndex :: Int
    , quoteList :: Array ChooseVehicleController.Config
    , showTollExtraCharges :: Boolean
    , nearByDrivers :: Maybe Int
    , showPreferences :: Boolean
    , bookingPreferenceEnabled :: Boolean
    , flowWithoutOffers :: Boolean
    , enableSingleEstimate :: Boolean
    , selectedEstimateHeight :: Int
    , zoneType :: ZoneType
    , tipViewProps :: TipViewProps
    , tipForDriver :: Int
    , customerTipArray :: Array String
    , customerTipArrayWithValues :: Array Int
    , enableTips :: Boolean
    , intercity :: Boolean
    }

config :: Config
config =
  { rideDistance: ""
  , rideDuration: ""
  , activeIndex: 0
  , quoteList: []
  , showTollExtraCharges : (getAppConfig appConfig).searchLocationConfig.showAdditionalChargesText
  , nearByDrivers : Nothing
  , showPreferences : false
  , bookingPreferenceEnabled : false
  , flowWithoutOffers : false
  , enableSingleEstimate : false
  , selectedEstimateHeight : 0
  , zoneType : NOZONE
  , customerTipArray : []
  , customerTipArrayWithValues : []
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
  , intercity : false
  }
