{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.ComponentConfig where

import Language.Strings
import PrestoDOM

import Common.Types.App as Common
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.MobileNumberEditor as MobileNumberEditor
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Types (STR(..))
import Prelude (not,(<>))
import Resource.Constants as Constant
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonViewConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CONTINUE) }
      , id = "PrimaryButtonMobileNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.6
      , height = (V 60)
      , cornerRadius = 0.0
      , margin = (Margin 0 0 0 0)
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in primaryButtonConfig'

mobileNumberButtonConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
mobileNumberButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CONTINUE) }
      , id = "PrimaryButtonMobileNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.4
      , margin = (Margin 0 0 0 0 )
      , enableLoader = (JB.getBtnLoader "PrimaryButtonMobileNumber")
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in primaryButtonConfig'

mobileNumberEditTextConfig :: ST.EnterMobileNumberScreenState -> PrimaryEditText.Config
mobileNumberEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        {
            color = Color.black800
          , singleLine = true
          , pattern = Just "[0-9]*,10"
          , margin = MarginHorizontal 10 10
          , focused = state.props.mobileNumberEditFocused
        }
      , background = Color.white900
      , topLabel
        { text = "Enter your Mobile number"
        , color = Color.black800
        , alpha = 0.8
        }
      , id = (EHC.getNewIDWithTag "EnterMobileNumberEditText")
      , type = "number"
      , height = V 54
      , margin = MarginTop 30
      , showErrorLabel = (not state.props.isValid)
      , errorLabel
        { text = (getString INVALID_MOBILE_NUMBER)
        }
      , showConstantField = true
      , constantField { 
         padding = PaddingBottom 1
        }
      }
    in primaryEditTextConfig'

mobileNumberConfig :: ST.EnterMobileNumberScreenState -> MobileNumberEditor.Config
mobileNumberConfig state = let 
  config = MobileNumberEditor.config 
  mobileNumberEditor' = config 
    { editText
      { color = Color.black800
      , singleLine = true 
      , pattern = Just "[0-9]*,10"
      , margin = MarginHorizontal 10 0
      , text = ""
      , placeholder = getString TEN_DIGIT_MOBILE_NUMBER
      , padding = Padding 0 16 16 16
      , focused = state.props.mobileNumberEditFocused
      }
    , showCountryCodeField = false
    , topLabel
      { text = (getString ENTER_YOUR_MOBILE_NUMBER)
      , color = Color.black800
      , accessibility = DISABLE
      , textStyle = FontStyle.SubHeading1
      }
    , type = "number"
    , id = (EHC.getNewIDWithTag "EnterMobileNumberEditText")
    , errorLabel
        { text = (getString INVALID_MOBILE_NUMBER)
        , margin = MarginBottom 1
        }
    , showErrorLabel = state.props.isValid
    , background = state.data.config.themeColors.radioInactiveBackground
    , focusedStroke = ("1," <> state.data.config.themeColors.editTextFocusedStroke)
    , stroke = ("1," <> state.data.config.themeColors.radioInactiveBackground)
    }
  in mobileNumberEditor'
