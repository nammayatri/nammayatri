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
import Data.Maybe (Maybe(..), isNothing)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Types (STR(..))
import Prelude (not, (==), (||))
import Resource.Constants as Constant
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))

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


googleProvider :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
googleProvider state = PrimaryButton.config
    { textConfig{ text = "Continue with Google"
      , color = Color.blue500 }
      , id = "PrimaryButtonGoogleAuth"
      , margin = MarginTop 16
      , isPrefixImage = true
      , prefixImageConfig {
          height = V 25
        , width = V 25
        , margin = MarginRight 10
        , imageUrl = fetchImage FF_ASSET "ny_ic_google"
      }
      , background = Color.bridgeGreen
    }

appleProvider :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
appleProvider state = PrimaryButton.config
    { textConfig{ text = "Continue with Apple"
      , color = Color.white900 }
      , id = "PrimaryButtonAppleAuth"
      , margin = MarginTop 12
      , isPrefixImage = true
      , prefixImageConfig {
          height = V 25
        , width = V 25
        , margin = MarginRight 10
        , imageUrl = fetchImage FF_ASSET "ny_ic_apple"
      }
      , background = Color.black
    }


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
  paddingOffest = EHC.os == "IOS"
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
    , countryCodeOptionConfig {
        padding = Padding 16 ((if EHC.os == "IOS" then 16 else 12)) 8 (if EHC.os == "IOS" then 16 else 12)
    }
    }
  in mobileNumberEditor'


emailIdPrimaryEditTextConfig :: ST.EnterMobileNumberScreenState -> PrimaryEditText.Config
emailIdPrimaryEditTextConfig state = 
  PrimaryEditText.config
      { editText
        { color = Color.black800
        , textStyle = FontStyle.Body1
        , margin = (Margin 16 16 16 16)
        , placeholder = "example@xyz.com"
        }
      , background = Color.white900
      , showErrorLabel = not (state.props.isValid || isNothing state.data.email)
      , topLabel
        { visibility = GONE
        }  
      , margin = MarginTop 0
      , errorLabel{
          text = "Enter a Valid Email"
        , color = Color.textDanger
        }
      }

primaryButtonEmailConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
primaryButtonEmailConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Login with Email" }
      , id = "PrimaryButtonMobileNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.6
      , margin = MarginTop 8
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in primaryButtonConfig'