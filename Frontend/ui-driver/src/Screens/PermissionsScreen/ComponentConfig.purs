{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionsScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color
import Data.Maybe (Maybe(..))
import Components.StepsHeaderModel as StepsHeaderModel
import Components.PopUpModal as PopUpModal

primaryButtonConfig :: ST.PermissionsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
      { text = (getString CONTINUE)
      , color = Color.primaryButtonColor
      , textSize = FontSize.a_18}
      , margin = (Margin 15 0 15 30)
      , cornerRadius = 8.0
      , background = Color.black900
      , height = (V 50)
      , alpha = if(state.props.isLocationPermissionChecked && state.props.isOverlayPermissionChecked && state.props.isAutoStartPermissionChecked) then 1.0 else 0.7
      , isClickable = (state.props.isLocationPermissionChecked && state.props.isOverlayPermissionChecked && state.props.isAutoStartPermissionChecked)
      }
  in primaryButtonConfig'

stepsHeaderModelConfig ::ST.PermissionsScreenState -> StepsHeaderModel.Config
stepsHeaderModelConfig state = let
    config = StepsHeaderModel.config 7
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false
    , driverMobileNumber = Just state.data.driverMobileNumber
     }
  in stepsHeaderConfig'

logoutPopUp :: ST.PermissionsScreenState -> PopUpModal.Config
logoutPopUp  state = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    option1 {text = (getString LOGOUT)},
    option2 {text = (getString CANCEL)},
    onBoardingButtonVisibility = true
  }
  in popUpConfig'