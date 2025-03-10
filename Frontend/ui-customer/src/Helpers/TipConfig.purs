module Helpers.TipConfig where

import Prelude
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Array (mapWithIndex, (!!), elem)
import Data.Maybe 
import Data.String as DS
import Helpers.Utils as HU
import Storage (getValueToLocalStore, KeyStore(..))
import Screens.Types (City(..), TipViewProps(..), TipViewStage(..), TipViewData(..))
import Locale.Utils
import MerchantConfig.Types
import Effect (Effect)
import Foreign.Class (class Encode)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Foreign.Generic (decodeJSON, encodeJSON)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))
import RemoteConfig (getTipConfigRC)

type TipConfig = {
  customerTipArray :: Array String,
  customerTipArrayWithValues :: Array Int
} 

type TipVehicleConfig = {
  sedan :: TipConfig,
  suv :: TipConfig,
  hatchback :: TipConfig,
  autoRickshaw :: TipConfig,
  taxi :: TipConfig,
  taxiPlus :: TipConfig,
  bike :: TipConfig,
  suvPlus :: TipConfig,
  heritageCab :: TipConfig
}


getTipConfig :: String -> TipConfig
getTipConfig variant = do
  let city = getValueToLocalStore CUSTOMER_LOCATION
      tipsConfig = getTipConfigRC $ DS.toLower city
  case variant of
    "SEDAN" -> mkTipConfig tipsConfig.sedan
    "SUV" -> mkTipConfig tipsConfig.suv
    "HATCHBACK" -> mkTipConfig tipsConfig.hatchback
    "AUTO_RICKSHAW" -> mkTipConfig tipsConfig.autoRickshaw
    "TAXI" -> mkTipConfig tipsConfig.taxi
    "TAXI_PLUS" -> mkTipConfig tipsConfig.taxiPlus
    "BOOK_ANY" -> mkTipConfig tipsConfig.bookAny
    "AMBULANCE_TAXI" -> mkTipConfig tipsConfig.ambulanceTaxi
    "AMBULANCE_TAXI_OXY" -> mkTipConfig tipsConfig.ambulanceTaxiOxy
    "AMBULANCE_AC_OXY" -> mkTipConfig tipsConfig.ambulanceAcOxy
    "AMBULANCE_AC" -> mkTipConfig tipsConfig.ambulanceAc
    "AMBULANCE_VENTILATOR" -> mkTipConfig tipsConfig.ambulanceVentilator
    "BIKE" -> mkTipConfig tipsConfig.bike
    "HERITAGE_CAB" -> mkTipConfig tipsConfig.heritageCab
    "DELIVERY_TRUCK_MINI" -> mkTipConfig tipsConfig.deliveryTruckMini
    "DELIVERY_TRUCK_SMALL" -> mkTipConfig tipsConfig.deliveryTruckSmall
    "DELIVERY_TRUCK_MEDIUM" -> mkTipConfig tipsConfig.deliveryTruckMedium
    "DELIVERY_TRUCK_LARGE" -> mkTipConfig tipsConfig.deliveryTruckLarge
    "DELIVERY_TRUCK_ULTRA_LARGE" -> mkTipConfig tipsConfig.deliveryTruckUltraLarge
    _ -> mkTipConfig tipsConfig.default

mkTipConfig :: Array Int -> TipConfig
mkTipConfig customerTipArrayWithValues = {
  customerTipArray: getTips customerTipArrayWithValues,
  customerTipArrayWithValues: customerTipArrayWithValues
}

getTips :: Array Int -> Array String
getTips arr = mapWithIndex (\index item -> if item == 0 then (getString NO_TIP) 
                                           else "₹" <> show item <> " " <> fromMaybe "🤩" (emoji !! index)) arr
  where
    emoji = [(getString NO_TIP), "🙂", "😀", "😃", "😁", "🤩"]

yatriSathiConfig :: String -> TipConfig
yatriSathiConfig variant = 
  case variant of
    "BIKE" -> mkTipConfig [0, 10, 20, 30]
    "AUTO_RICKSHAW" -> mkTipConfig [0, 10, 20, 30]
    _ | HU.isAmbulance variant -> mkTipConfig []
    _ -> mkTipConfig [0, 20, 30, 50]

getTipViewProps :: TipViewProps -> String -> TipViewProps
getTipViewProps tipViewProps vehicleVariant = do
  case tipViewProps.stage of
    DEFAULT ->  tipViewProps{ stage = DEFAULT
                            , onlyPrimaryText = false
                            , isprimaryButtonVisible = false
                            , primaryText = getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER
                            , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                            }
    TIP_AMOUNT_SELECTED -> tipViewProps{ stage = TIP_AMOUNT_SELECTED
                                       , onlyPrimaryText = false
                                       , isprimaryButtonVisible = true
                                       , primaryText = getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER
                                       , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                                       , primaryButtonText = getTipViewText tipViewProps vehicleVariant (getString CONTINUE_SEARCH_WITH)
                                       }
    TIP_ADDED_TO_SEARCH -> tipViewProps{ onlyPrimaryText = true, isprimaryButtonVisible = false, primaryText = (getTipViewText tipViewProps vehicleVariant (getString SEARCHING_WITH)) <> "." }
    RETRY_SEARCH_WITH_TIP -> tipViewProps{ onlyPrimaryText = true , isprimaryButtonVisible = false, primaryText = (getTipViewText tipViewProps vehicleVariant (getString SEARCHING_WITH)) <> "." }

getTipViewText :: TipViewProps -> String-> String -> String
getTipViewText tipViewProps vehicleVariant prefixString = do
  let tipConfig = getTipConfig vehicleVariant
      tip = show (fromMaybe 10 (tipConfig.customerTipArrayWithValues !! tipViewProps.activeIndex))
  if tip == "0" then 
    case tipViewProps.stage of
      TIP_AMOUNT_SELECTED -> getString CONTINUE_SEARCH_WITH_NO_TIP
      _ -> getString SEARCHING_WITH_NO_TIP
  else  
    case (getLanguageLocale languageKey) of
      "EN_US" -> prefixString <> (if tipViewProps.stage == TIP_AMOUNT_SELECTED then " +₹" else " ₹")<>tip<>" "<> (getString TIP)
      _ -> "+₹"<>tip<>" "<>(getString TIP) <> " " <> prefixString

isTipEnabled :: CustomerTip -> String -> Boolean
isTipEnabled tipConfig vehicleVariant =
  case vehicleVariant of 
    "AUTO_RICKSHAW" -> tipConfig.auto
    _ -> tipConfig.cabs

setTipViewData :: Encode TipViewData => TipViewData -> Effect Unit
setTipViewData object = void $ pure $ setValueToLocalStore TIP_VIEW_DATA (encodeJSON object)
