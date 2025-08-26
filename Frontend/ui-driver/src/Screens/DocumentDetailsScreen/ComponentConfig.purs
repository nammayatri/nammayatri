module Screens.DocumentDetailsScreen.ComponentConfig where

import Prelude
import Language.Strings
import PrestoDOM

import Common.Types.App as Common
import Components.PopUpModal as PopUpModal
import Components.PopUpModal.Controller as PopUpModalConfig
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resource.Constants as Constant
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils as HU
import Data.Array as DA
import Storage ( getValueToLocalStore , KeyStore(..))
import ConfigProvider
import Mobility.Prelude

appOnboardingNavBarConfig :: ST.DocumentDetailsScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { genericHeaderConfig = genericHeaderConfig state,
    appConfig = state.data.config,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig
              { color = state.data.config.themeColors.onboardingHeaderTextColor,
                text = "Documents"
              },
    rightButton = AppOnboardingNavBar.config.rightButton{
      text = getString HELP_FAQ,
      color = state.data.config.themeColors.onboardingHeaderTextColor
      },
    navBarOpen = state.props.menuOptions,
    prefixImageConfig = AppOnboardingNavBar.config.prefixImageConfig{ image = state.data.config.themeColors.defaultBackButton }
  }


genericHeaderConfig :: ST.DocumentDetailsScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig {
       visibility = GONE
      }
    , padding = (PaddingVertical 5 5)
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'