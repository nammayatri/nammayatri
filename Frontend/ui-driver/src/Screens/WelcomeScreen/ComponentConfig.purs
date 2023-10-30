module Screens.WelcomeScreen.ComponentConfig where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types (WelcomeScreenStage(..), WelcomeScreenState)
import Styles.Colors as Color

---------------- genericHeaderConfig ----------------
genericHeaderConfig :: WelcomeScreenState -> GenericHeader.Config
genericHeaderConfig state =
  GenericHeader.config
    { height = WRAP_CONTENT
    , prefixImageConfig
      { height = (V 30)
      , width = (V 30)
      , margin = (MarginRight 16)
      , imageUrl = "ny_ic_chevron_left," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
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

primaryButtonConfig :: WelcomeScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = getString case state.props.currentStage of
                                        SELECT_LANG ->  CONFIRM_LOCATION_STR
                                        SELECT_CITY ->  CONFIRM_LANGUAGE
                                        CAROUSEL -> GET_STARTED
                                        ENABLE_LOCATION -> GET_STARTED
                                        _ -> GET_STARTED

        }
      , id = "PrimaryButtonWelcomeScreen"
      }
  in primaryButtonConfig'