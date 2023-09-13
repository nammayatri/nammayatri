{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Components.OtpPrimaryEditText.Controller as PrimaryEditTextController
import Styles.Colors as Color
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Prelude ((<>))
import Data.Maybe (Maybe(..))
import Font.Style as FontStyle
import Components.StepsHeaderModel as StepsHeaderModel
import JBridge as JB
import Common.Types.App
import Font.Size as FontSize
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Components.StepsHeaderModel as StepsHeaderModel
import Screens.Types as ST

primaryButtonViewConfig :: ST.EnterOTPScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString REGISTER) }
      , id = "PrimaryButtonOtpNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.6
      , height = (V 60)
      , cornerRadius = 0.0
      , margin = (Margin 0 0 0 0)
      }
  in primaryButtonConfig'


verifyOTPButtonConfig :: ST.EnterOTPScreenState -> PrimaryButton.Config
verifyOTPButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CONTINUE) }
      , id = "PrimaryButtonOTP"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.4
      , margin = (Margin 0 0 0 0 )
      , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
      }
  in primaryButtonConfig'

stepsHeaderModelConfig :: ST.EnterOTPScreenState -> StepsHeaderModel.Config
stepsHeaderModelConfig state = let
    config = StepsHeaderModel.config 0
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false,
      profileIconVisibility = false,
      driverNumberVisibility = false,
      logoutVisibility = false,
      activeIndex = 1
     }
  in stepsHeaderConfig'

otpEditTextConfig :: ST.EnterOTPScreenState ->PrimaryEditTextController.Config
otpEditTextConfig state = let 
    config = PrimaryEditTextController.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
       -- , placeholder = "Enter 4 Digit OTP"--(getString ENTER_4_DIGIT_OTP)
        , singleLine = true
        , pattern = Just "[0-9]*,4"
        , margin = MarginHorizontal 10 10
        , fontStyle = FontStyle.bold LanguageStyle
        , textSize = FontSize.a_22
        , letterSpacing = PX 18.0
        --, letterSpacing = state.props.letterSpacing
        , text = ""
       -- , focused = state.props.otpEdtFocused
        , gravity = LEFT
        }
      , background = Color.white900
      , margin = (Margin 0 30 0 20)
      , topLabel
        { textSize = FontSize.a_14
        , text = (getString OTP_SENT_TO) <> " +91 " <> state.data.mobileNo
        , color = Color.black900
        , fontStyle = FontStyle.regular LanguageStyle
        , alpha = 0.8
        } 
      , id = state.data.editTextId
      , type = "number"
      , height = V 65
      , errorLabel {
            text = (getString WRONG_OTP),
            visibility = VISIBLE,
            textSize = FontSize.a_12,
            fontStyle = FontStyle.bold LanguageStyle,
            alpha = 0.8
      },
      width = MATCH_PARENT,
      showErrorLabel = state.props.isValid, 
      stroke = if state.props.isValid then ("1," <> Color.warningRed) else ("1," <> Color.borderColorLight)
      }
    in primaryEditTextConfig'