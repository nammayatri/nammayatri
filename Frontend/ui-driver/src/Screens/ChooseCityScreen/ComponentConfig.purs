module Screens.ChooseCityScreen.ComponentConfig where

import Prelude

import Common.Types.App (LazyCheck(..))
import Common.Types.App (YoutubeData, CarouselModal)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Data.Maybe as Mb
import Helpers.Utils (fetchImage, FetchImageFrom(..), getCityConfig)
import Language.Strings (getString)
import Resource.Localizable.StringsV2 (getString) as StringsV2
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), clickable)
import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Styles.Colors as Color
import Components.ErrorModal as ErrorModal
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.String.Common as DSC
import MerchantConfig.Types (AppConfig)
import Components.SelectMenuButton as MenuButton
import MerchantConfig.Types as MT

---------------- genericHeaderConfig ----------------
genericHeaderConfig :: ChooseCityScreenState -> GenericHeader.Config
genericHeaderConfig state =
  GenericHeader.config
    { height = WRAP_CONTENT
    , prefixImageConfig
      { height = (V 30)
      , width = (V 30)
      , margin = (MarginRight 16)
      , imageUrl = fetchImage FF_ASSET state.data.config.themeColors.defaultBackButton
      , padding = (Padding 5 5 5 5)
      , enableRipple = true
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
    isEnabled = case state.props.currentStage of
                  SELECT_LANG -> state.props.radioMenuFocusedLang /= ""
                  SELECT_CITY -> state.props.radioMenuFocusedCity /= ""
                  DETECT_LOCATION -> Mb.isJust state.data.locationSelected
                  _ -> true
    primaryButtonConfig' = config 
      { textConfig { text = getString case state.props.currentStage of
                                        SELECT_LANG ->  CONFIRM_LANGUAGE
                                        SELECT_CITY ->  CONFIRM_LOCATION_STR
                                        ENABLE_PERMISSION -> ENABLE_LOCATION
                                        _ -> CONFIRM

        }
      , alpha = if isEnabled then 1.0 else 0.5
      , isClickable =  if isEnabled then true else false
      , id = "PrimaryButtonChooseCityScreen"
      }
  in primaryButtonConfig'

getLangFromVal :: String -> String
getLangFromVal value =
  case value of
      "EN_US" -> "English"
      "HI_IN" -> "हिंदी"
      "TA_IN" -> "தமிழ்"
      "KN_IN" -> "ಕನ್ನಡ"
      "TE_IN" -> "తెలుగు"
      "BN_IN" -> "বাংলা"
      "ML_IN" -> "മലയാളം"
      _ -> value
-- getLocationMapImage :: Maybe String -> String
-- getLocationMapImage value =
--   case value of 
--     Mb.Just val -> case val of
--                   "Delhi" -> "ny_ic_delhi_map"
--                   "Hyderabad" -> "ny_ic_hyderabad_map"
--                   "Mysore" -> "ny_ic_mysuru_map"
--                   "Bangalore" -> "ny_ic_bangalore_map"
--                   "Chennai" -> "ny_ic_chennai_map"
--                   "Coimbatore" -> "ny_ic_coimbatore_map"
--                   _ -> "ny_ic_driver_location_undetectable"
--     Mb.Nothing -> "ny_ic_driver_location_undetectable"

getLocationMapImage :: ChooseCityScreenState -> String
getLocationMapImage state =
  if shouldShowUndetectable then "ny_ic_driver_location_undetectable" else cityConfig.mapImage
  where 
    cityConfig = getCityConfig state.data.config.cityConfig $ Mb.fromMaybe "" state.data.locationSelected
    shouldShowUndetectable = state.props.locationUnserviceable || state.props.locationDetectionFailed || DSC.null cityConfig.mapImage

getChangeLanguageText :: Maybe String -> AppConfig -> String
getChangeLanguageText value config = 
  StringsV2.getString cityConfig.languageKey CHANGE_LANGUAGE_STR 
  where 
    cityConfig = getCityConfig config.cityConfig $ Mb.fromMaybe "" value



mockLocationConfig :: ChooseCityScreenState -> ErrorModal.Config
mockLocationConfig state =
  ErrorModal.config
        { height = MATCH_PARENT
        , background = Color.white900
        , stroke = ("1," <> Color.borderGreyColor)
        , imageConfig
          { imageUrl = fetchImage FF_ASSET "ny_ic_location_unserviceable"
          , height = V 99
          , width = V 133
          , margin = MarginVertical 50 20
          }
        , errorConfig
          { text = getString UNABLE_TO_GET_YOUR_LOCATION
          , color = Color.black800
          , margin = MarginBottom 5
          }
        , errorDescriptionConfig
          { text = getString TURN_OFF_ANY_MOCK_LOCATION_APP_AND_RESTART
          , color = Color.black700
          , margin = Margin 20 0 20 40
          }
        , buttonConfig
          { visibility = GONE }
        }
menuButtonConfig :: Int -> MT.Language -> String -> ChooseCityScreenState -> MenuButton.State
menuButtonConfig index language selectedVal state = MenuButton.config { 
  text =
    { name: language.name
    , value: language.value
    , subtitle: language.subtitle
    }, 
  isSelected = selectedVal == language.value,
  index = index, 
  lineVisibility = false, 
  activeStrokeColor = state.data.config.themeColors.radioActiveStroke,
  activeBgColor = state.data.config.themeColors.radioActiveBackground,
  inactiveStrokeColor = Color.grey100,
  inactiveBgColor = state.data.config.themeColors.radioInactiveBackground,
  radioSelectedImage = state.data.config.themeColors.radioSelectedImage
  }