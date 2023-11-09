{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.ComponentConfig where

import Language.Strings
import PrestoDOM

import Common.Types.App as Common
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModal as StepsHeaderModel
import Data.Maybe (Maybe(..))
import Font.Style as FontStyle
import Language.Types (STR(..))
import Resource.Constants as Constant
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.RegistrationScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString NEXT)}
      , width = MATCH_PARENT
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , height = (V 60)
      }
  in primaryButtonConfig'

stepsHeaderModelConfig :: ST.RegistrationScreenState -> StepsHeaderModel.Config
stepsHeaderModelConfig state = let
    config = StepsHeaderModel.config 2
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false,
      profileIconVisibility = true,
      driverNumberVisibility = true,
      driverMobileNumber = (Just state.data.phoneNumber),
      customerTextArray = [],
      driverTextArray = Constant.driverTextArray Common.FunctionCall,
      rightButtonText = getString LOGOUT,
      logoutVisibility = true,
      backArrowVisibility = false
     }
  in stepsHeaderConfig'

logoutPopUp :: Common.LazyCheck -> PopUpModal.Config
logoutPopUp  dummy = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    buttonLayoutMargin = (MarginBottom 40),
    padding = (Padding 16 16 16 0),
    backgroundClickable = true,
    dismissPopup = true,
    option1 {
      text = (getString LOGOUT),
      color = Color.black700,
      textStyle = FontStyle.SubHeading1,
      strokeColor = Color.white900,
      width = MATCH_PARENT,
      height = WRAP_CONTENT,
      background = Color.blue600,
      margin = (MarginBottom 12),
      padding = PaddingVertical 16 16
      },
    option2 {
      text = (getString CANCEL),
      color = Color.black700,
      textStyle = FontStyle.SubHeading1,
      height = WRAP_CONTENT,
      strokeColor = Color.white900,
      width = MATCH_PARENT,
      padding = PaddingVertical 16 16,
      margin = (MarginBottom 0),
      background = Color.blue600
      },
    optionButtonOrientation = "VERTICAL"
  }
  in popUpConfig'