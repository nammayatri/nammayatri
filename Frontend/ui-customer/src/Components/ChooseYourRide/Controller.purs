module Components.ChooseYourRide.Controller where

import Data.Maybe (Maybe(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.PrimaryButton.Controller as PrimaryButtonController
import ConfigProvider
import MerchantConfig.Types
import Screens.Types(TipViewProps, TipViewStage(..), ZoneType(..), FareProductType(..))
import Prelude
import Resources.Constants (intMin, intMax)
import Components.ChooseVehicle as ChooseVehicle
import RemoteConfig as RC
import Data.String as DS
import Data.Ord(min, max)
import Data.Array (length, (!!), filter, any, foldl, elem)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Data.Int (fromString)
import Storage (getValueToLocalStore, KeyStore(..))
import JBridge
import DecodeUtil
import LocalStorage.Cache
import Data.Function.Uncurried (Fn2, runFn2, runFn3, Fn3, Fn1)
import Storage

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
    , suggestedActiveIndex : Nothing
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

getBookAnyProps :: Array ChooseVehicle.Config -> BookAnyProps
getBookAnyProps estimates = foldl (\acc item -> getMinMax acc item) bookAnyProps estimates
  where 
    getMinMax :: BookAnyProps -> ChooseVehicle.Config -> BookAnyProps
    getMinMax bookAnyProps item = 
      let minPrice = bookAnyProps.minPrice `min` (fromMaybe item.basePrice item.minPrice)
          maxPrice = bookAnyProps.maxPrice `max` (fromMaybe item.basePrice item.maxPrice)
          minCapacity = bookAnyProps.minCapacity `min` (fromMaybe 0 (fromString item.capacity))
          maxCapacity = bookAnyProps.maxCapacity `max` (fromMaybe 0 (fromString item.capacity))
      in bookAnyProps{minPrice = minPrice, maxPrice = maxPrice, minCapacity = minCapacity, maxCapacity = maxCapacity}

getMinMaxPrice :: BookAnyProps -> ChooseVehicle.Config -> Array ChooseVehicle.Config -> String
getMinMaxPrice bookAnyProps estimate estimates =
  let currency = getCurrency appConfig
  in case (length estimates), estimate.vehicleVariant == "BOOK_ANY" of 
      0, true -> "-"
      _, true -> if bookAnyProps.minPrice == bookAnyProps.maxPrice then (currency <> (show bookAnyProps.minPrice))
                 else (currency <> (show bookAnyProps.minPrice) <> " - " <> currency <> (show bookAnyProps.maxPrice))
      _ , false -> estimate.price
      _,_ -> "-"

getMinMaxCapacity :: BookAnyProps -> ChooseVehicle.Config -> Array ChooseVehicle.Config -> String
getMinMaxCapacity bookAnyProps estimate estimates =
  case (length estimates), estimate.vehicleVariant == "BOOK_ANY" of 
    0, true -> "-"
    _, true -> if bookAnyProps.minCapacity == bookAnyProps.maxCapacity then (show bookAnyProps.minCapacity)
               else (show bookAnyProps.minCapacity) <> " - " <> (show bookAnyProps.maxCapacity)
    _ , false -> estimate.capacity
    _,_ -> "-"