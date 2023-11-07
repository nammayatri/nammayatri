module Screens.ChooseCityScreen.ComponentConfig where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Styles.Colors as Color
import Common.Types.App (YoutubeData, CarouselModal)

---------------- genericHeaderConfig ----------------
genericHeaderConfig :: ChooseCityScreenState -> GenericHeader.Config
genericHeaderConfig state =
  GenericHeader.config
    { height = WRAP_CONTENT
    , prefixImageConfig
      { height = (V 30)
      , width = (V 30)
      , margin = (MarginRight 16)
      -- , imageUrl = "ny_ic_chevron_left," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
      , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
      , padding = (Padding 5 5 5 5)
      }
    , textConfig
      { text = getString if state.props.currentStage == SELECT_LANG then SELECT_LANGUAGE else SELECT_LOCATION
      , color = Color.black
      , margin = (MarginVertical 12 12)
      }
    , suffixImageConfig
      { visibility = GONE
      }
    }

primaryButtonConfig :: ChooseCityScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = getString case state.props.currentStage of
                                        SELECT_LANG ->  CONFIRM_LANGUAGE
                                        SELECT_CITY ->  CONFIRM_LOCATION_STR
                                        -- CAROUSEL -> GET_STARTED
                                        ENABLE_PERMISSION -> ENABLE_LOCATION
                                        _ -> CONFIRM

        }
      -- , alpha = 0.3
      , id = "PrimaryButtonChooseCityScreen"
      }
  in primaryButtonConfig'

getLangFromVal :: String -> String
getLangFromVal value =
  case value of
      "EN_US" -> "English"
      "HI_IN" -> "Hindi"
      "TA_IN" -> "Tamil"
      "KN_IN" -> "Kannada"
      "TE_IN" -> "Telugu"
      "BN_IN" -> "Bengali"
      "ML_IN" -> "Malayalam"
      _ -> value

getLocationMapImage :: String -> String
getLocationMapImage value =
  case value of
    "Delhi" -> "ny_ic_delhi_map"
    "Hyderabad" -> "ny_ic_hyderabad_map"
    "Mysore" -> "ny_ic_mysuru_map"
    "Bengaluru" -> "ny_ic_bangalore_map"
    _ -> "ny_ic_driver_location_undetectable"