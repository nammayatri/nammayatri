module Helpers.TipConfig where

import Prelude
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Array (mapWithIndex, (!!), elem)
import Data.Maybe 
import Data.String as DS
import Helpers.Utils as HU
import Data.Int
import Storage (getValueToLocalStore, KeyStore(..))
import Screens.Types (TipViewProps(..), TipViewStage(..), TipViewData(..))
import Locale.Utils
import MerchantConfig.Types
import Effect (Effect)
import Foreign.Class (class Encode)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Foreign.Generic (decodeJSON, encodeJSON)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (City(..), LazyCheck(..))
import RemoteConfig (getTipConfigRC)
import Debug (spy)
import Engineering.Helpers.Utils as EHU

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
  evAutoRickshaw :: TipConfig,
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
    "EV_AUTO_RICKSHAW" -> mkTipConfig tipsConfig.evAutoRickshaw
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
    _ | EHU.isAmbulance variant -> mkTipConfig []
    _ -> mkTipConfig [0, 20, 30, 50]

getTipViewProps :: TipViewProps -> String -> Maybe String -> Maybe Int -> TipViewProps
getTipViewProps tipViewProps vehicleVariant smartTipReason smartTipSuggestion = do
  let smartTipSuggestionValue = fromMaybe 10 smartTipSuggestion
      tipConfig = getTipConfig vehicleVariant
      customerTipArrWithValues = if smartTipSuggestion == Nothing then tipConfig.customerTipArrayWithValues else if smartTipSuggestionValue <= 10 then [0, 10, 20, 30] else [0, roundUpToNearest10 (smartTipSuggestionValue `div` 2), smartTipSuggestionValue, roundUpToNearest10 (ceil (toNumber smartTipSuggestionValue * 1.5))]
      customerTipArray = if smartTipSuggestion == Nothing then tipConfig.customerTipArray else getTips customerTipArrWithValues
      suggestedActiveIndex = if smartTipSuggestion == Nothing then Nothing else if smartTipSuggestionValue <= 10 then Just 1 else Just 2  
      tipViewPropsModified = tipViewProps{suggestedActiveIndex = suggestedActiveIndex, customerTipArray = if tipViewProps.customerTipArray == [] then customerTipArray else tipViewProps.customerTipArray, customerTipArrayWithValues = if tipViewProps.customerTipArrayWithValues == [] then customerTipArrWithValues else tipViewProps.customerTipArrayWithValues}
  case tipViewProps.stage of
    DEFAULT -> do
      tipViewProps{ stage = if smartTipSuggestion == Nothing then DEFAULT else TIP_AMOUNT_SELECTED
                            , onlyPrimaryText = false
                            , isprimaryButtonVisible = if smartTipSuggestion == Nothing then false else true
                            , primaryText = fromMaybe (getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER) smartTipReason
                            , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                            , customerTipArray = customerTipArray
                            , customerTipArrayWithValues = customerTipArrWithValues
                            , primaryButtonText = getTipViewText tipViewPropsModified vehicleVariant (getString CONTINUE_SEARCH_WITH)
                            }
    TIP_AMOUNT_SELECTED -> tipViewPropsModified{ stage = TIP_AMOUNT_SELECTED
                                       , onlyPrimaryText = false
                                       , isprimaryButtonVisible = true
                                       , primaryText = fromMaybe (getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER) smartTipReason
                                       , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                                       , primaryButtonText = getTipViewText tipViewPropsModified vehicleVariant (getString CONTINUE_SEARCH_WITH)
                                       }
    TIP_ADDED_TO_SEARCH -> tipViewPropsModified { onlyPrimaryText = true, isprimaryButtonVisible = false, primaryText = (getTipViewText tipViewPropsModified vehicleVariant (getString SEARCHING_WITH)) <> "." }
    RETRY_SEARCH_WITH_TIP -> tipViewPropsModified { onlyPrimaryText = true , isprimaryButtonVisible = false, primaryText = (getTipViewText tipViewPropsModified vehicleVariant (getString SEARCHING_WITH)) <> "." }

getTipViewText :: TipViewProps -> String-> String -> String
getTipViewText tipViewProps vehicleVariant prefixString = do
  let tipConfig = getTipConfig vehicleVariant
      tip = show (fromMaybe 0 (tipViewProps.customerTipArrayWithValues !! tipViewProps.activeIndex))
  if tip == "0" then 
    case tipViewProps.stage of
      TIP_AMOUNT_SELECTED -> getString CONTINUE_SEARCH_WITH_NO_TIP
      DEFAULT -> getString CONTINUE_SEARCH_WITH_NO_TIP
      _ -> getString SEARCHING_WITH_NO_TIP
  else  
    case (getLanguageLocale languageKey) of
      "EN_US" -> prefixString <> (if tipViewProps.stage == TIP_AMOUNT_SELECTED then " ₹" else " ₹")<>tip<>" "<> (getString TIP)
      _ -> "₹"<>tip<>" "<>(getString TIP) <> " " <> prefixString

isTipEnabled :: CustomerTip -> String -> Boolean
isTipEnabled tipConfig vehicleVariant =
  case vehicleVariant of 
    "AUTO_RICKSHAW" -> tipConfig.auto
    _ -> tipConfig.cabs

setTipViewData :: Encode TipViewData => TipViewData -> Effect Unit
setTipViewData object = void $ pure $ setValueToLocalStore TIP_VIEW_DATA (encodeJSON object)

roundUpToNearest10 :: Int -> Int
roundUpToNearest10 n = ((n + 9) `div` 10) * 10