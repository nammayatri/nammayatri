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
import Components.PrimaryEditText.Controller as PrimaryEditText
import Styles.Colors as Color
import Language.Strings
import PrestoDOM
import Prelude
import Common.Types.App as Common
import Language.Types (STR(..))
import Resource.Constants as Constant
import Data.Maybe (Maybe(..))
import Font.Style as FontStyle
import JBridge as JB
import Common.Types.App
import Font.Size as FontSize
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Screens.Types as ST
import Data.String as DS
import Locale.Utils

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


primaryEditTextConfig state = 
  PrimaryEditText.config { editText
        { color = Color.black800
        , placeholder = (getString AUTO_READING_OTP)
        , singleLine = true
        , pattern = Just "[0-9]*,4"
        , margin = MarginHorizontal 10 10
        , textStyle = FontStyle.Body7
        , letterSpacing = PX if state.data.otp == "" then 0.0 else 5.0
        , text = state.data.capturedOtp
        , focused = true
        , gravity = LEFT
        }
      , background = Color.white900
      , margin = (Margin 0 30 0 20)
      , topLabel
        { text = case (getLanguageLocale languageKey) of 
                  "EN_US" -> (getString ENTER_OTP_SENT_TO) <> if DS.null state.data.mobileNo then state.data.email else state.data.config.defaultCountryCodeConfig.countryCode <> " " <> state.data.mobileNo
                  _ -> (if DS.null state.data.mobileNo then state.data.email else state.data.config.defaultCountryCodeConfig.countryCode <> " " <> state.data.mobileNo) <> (getString ENTER_OTP_SENT_TO)
        , color = Color.black800
        , alpha = 0.8
        , accessibility = DISABLE
        } 
      , id = (EHC.getNewIDWithTag "EnterOTPScreenEditText")
      , type = "number"
      , height = V 54
      , errorLabel {
            text = getString PLEASE_ENTER_VALID_OTP,
            visibility = VISIBLE,
            alpha = 0.8
      },
      focusedStroke = "1," <> Color.borderColorLight,
      width = MATCH_PARENT,
      showErrorLabel = state.props.isValid, 
      stroke = if state.props.isValid then ("1," <> Color.warningRed) else ("1," <> Color.borderColorLight)
      }