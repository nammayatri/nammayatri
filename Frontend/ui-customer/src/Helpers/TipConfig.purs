module Helpers.TipConfig where

import Prelude
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Array (mapWithIndex, (!!), elem)
import Data.Maybe 
import Helpers.Utils as HU
import Storage (getValueToLocalStore, KeyStore(..))
import Screens.Types (City(..), TipViewProps(..), TipViewStage(..), TipViewData(..))
import Locale.Utils
import MerchantConfig.Types
import Effect (Effect)
import Foreign.Class (class Encode)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Foreign.Generic (decodeJSON, encodeJSON)


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
  taxiPlus :: TipConfig
}


getTipConfig :: String -> TipConfig
getTipConfig variant = do
  let city = HU.getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
  case city of 
    Bangalore -> bangaloreConfig variant
    Hyderabad -> hyderabadConfig variant
    _ -> defaultTipConfig variant

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
      
bangaloreConfig :: String -> TipConfig
bangaloreConfig variant = 
  case variant of
    "SEDAN" -> mkTipConfig []
    "SUV" -> mkTipConfig []
    "HATCHBACK" -> mkTipConfig []
    "AUTO_RICKSHAW" -> mkTipConfig [0, 10, 20, 30]
    "TAXI" -> mkTipConfig []
    "TAXI_PLUS" -> mkTipConfig []
    _ -> mkTipConfig []

hyderabadConfig :: String -> TipConfig
hyderabadConfig variant = 
  case variant of
    "SEDAN" -> mkTipConfig []
    "SUV" -> mkTipConfig []
    "HATCHBACK" -> mkTipConfig []
    "AUTO_RICKSHAW" -> mkTipConfig [0, 10, 20, 30]
    "TAXI" -> mkTipConfig []
    "TAXI_PLUS" -> mkTipConfig []
    _ -> mkTipConfig []

defaultTipConfig :: String -> TipConfig
defaultTipConfig variant = 
  case variant of
    "SEDAN" -> mkTipConfig []
    "SUV" -> mkTipConfig []
    "HATCHBACK" -> mkTipConfig []
    "AUTO_RICKSHAW" -> mkTipConfig [0, 10, 20, 30]
    "TAXI" -> mkTipConfig []
    "TAXI_PLUS" -> mkTipConfig []
    _ -> mkTipConfig []

getTipViewProps :: TipViewProps -> String -> TipViewProps
getTipViewProps tipViewProps vehicleVariant = do
  case tipViewProps.stage of
    DEFAULT ->  tipViewProps{ stage = DEFAULT
                            , onlyPrimaryText = false
                            , isprimaryButtonVisible = false
                            , primaryText = getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER
                            , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                            -- , secondaryButtonText = getString GO_BACK_
                            -- , secondaryButtonVisibility = true
                            }
    TIP_AMOUNT_SELECTED -> tipViewProps{ stage = TIP_AMOUNT_SELECTED
                                       , onlyPrimaryText = false
                                       , isprimaryButtonVisible = true
                                       , primaryText = getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER
                                       , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                                       , primaryButtonText = getTipViewText tipViewProps vehicleVariant (getString CONTINUE_SEARCH_WITH)
                                      --  , secondaryButtonText = getString GO_BACK_
                                      --  , secondaryButtonVisibility = true
                                       }
    TIP_ADDED_TO_SEARCH -> tipViewProps{ onlyPrimaryText = true, isprimaryButtonVisible = false, primaryText = (getTipViewText tipViewProps vehicleVariant (getString SEARCHING_WITH)) <> "." }
    RETRY_SEARCH_WITH_TIP -> tipViewProps{ onlyPrimaryText = true , isprimaryButtonVisible = false, primaryText = (getTipViewText tipViewProps vehicleVariant (getString SEARCHING_WITH)) <> "." }
    -- ADD_TIP_OR_CHANGE_RIDE_TYPE -> tipViewProps{ showTipsList = false
    --                                           , isprimaryButtonVisible = true
    --                                           , primaryText = getString TRY_ADDING_TIP_OR_CHANGE_RIDE_TYPE
    --                                           , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
    --                                           , primaryButtonText = getString ADD_TIP
    --                                           , secondaryButtonText = getString CHANGE_RIDE_TYPE
    --                                           , secondaryButtonVisibility = true
    --                                           }

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
