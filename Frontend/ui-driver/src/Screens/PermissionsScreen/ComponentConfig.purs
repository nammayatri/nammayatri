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
import Components.StepsHeaderModal as StepsHeaderModel
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Resource.Constants as Constant
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.PermissionsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    isEnabled = ((state.props.androidVersion < 13 || state.props.isNotificationPermissionChecked) && state.props.isOverlayPermissionChecked && state.props.isAutoStartPermissionChecked)
    primaryButtonConfig' = config 
      { textConfig
      { text = (getString CONTINUE)
      , color = Color.primaryButtonColor
      }
      , background = Color.black900
      , height = (V 50)
      , alpha = if isEnabled then 1.0 else 0.7
      , isClickable = isEnabled
      , id = "PermissionsScreenPrimaryButton"
      , margin = Margin 15 0 15 30
      , cornerRadius = 6.0
      }
  in primaryButtonConfig'

stepsHeaderModelConfig ::ST.PermissionsScreenState -> StepsHeaderModel.Config
stepsHeaderModelConfig state = let
    config = StepsHeaderModel.config 7
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false,
      profileIconVisibility = true,
      driverNumberVisibility = true,
      logoutVisibility = true,
      customerTextArray = [],
      driverTextArray = Constant.driverTextArray Common.FunctionCall,
      rightButtonText = getString LOGOUT,
      driverMobileNumber = Just state.data.driverMobileNumber
     }
  in stepsHeaderConfig'