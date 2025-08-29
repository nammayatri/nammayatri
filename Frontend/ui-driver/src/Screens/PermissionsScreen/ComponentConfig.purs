{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionsScreen.ComponentConfig where

import Language.Strings
import Prelude
import PrestoDOM

import Common.Types.App as Common
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Resource.Constants as Constant
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils as HU
import Data.Array as DA
import Storage (getValueToLocalStore, KeyStore(..))

primaryButtonConfig :: ST.PermissionsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    isEnabled = (state.props.isOverlayPermissionChecked && 
                state.props.isAutoStartPermissionChecked && 
                (state.props.androidVersion < 13 || state.props.isNotificationPermissionChecked || not state.data.config.permissions.notification) && 
                (not state.data.config.permissions.locationPermission || state.props.isLocationPermissionChecked) 
                )
    primaryButtonConfig' = config 
      { textConfig
      { text = (getString CONTINUE)
      }
      , height = (V 50)
      , alpha = if isEnabled then 1.0 else 0.7
      , isClickable = isEnabled
      , id = "PermissionsScreenPrimaryButton"
      , margin = Margin 15 0 15 30
      , cornerRadius = 6.0
      }
  in primaryButtonConfig'


genericHeaderConfig :: ST.PermissionsScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = HU.fetchImage HU.FF_ASSET "ic_new_avatar"
      , height = (V 25)
      , width = (V 25)
      , margin = (Margin 12 5 5 5)
      }
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = if DA.any (_ == getValueToLocalStore DRIVER_NAME) ["", "__failed"] then getValueToLocalStore MOBILE_NUMBER_KEY else getValueToLocalStore DRIVER_NAME
      , color = state.data.config.themeColors.onboardingHeaderTextColor
      , margin = MarginHorizontal 5 5 
      , textStyle = FontStyle.Body1
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

appOnboardingNavBarConfig :: ST.PermissionsScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { genericHeaderConfig = genericHeaderConfig state,
    appConfig = state.data.config,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig
              { text = "App Permissions"-- getString GRANT_PERMISSIONS
              , color = state.data.config.themeColors.onboardingHeaderTextColor
              },
    navBarOpen = state.props.logoutModalView,
    rightButton = AppOnboardingNavBar.config.rightButton{
      text = getString HELP_FAQ,
      color = state.data.config.themeColors.onboardingHeaderTextColor
      },
    prefixImageConfig = AppOnboardingNavBar.config.prefixImageConfig{ image = state.data.config.themeColors.defaultBackButton }
  }