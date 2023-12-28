{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.OnBoardingFlow.EnterMobileNumberScreen.ComponentConfig where

import Common.Types.App

import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.MobileNumberEditor as MobileNumberEditor
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (mobileNumberMaxLength)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (not, (<>), (==), (&&), (/=), show)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), visibility, Accessiblity(..))
import Screens.Types as ST
import Storage (KeyStore(..))
import Styles.Colors as Color
import Common.Types.App
import Storage(getValueToLocalStore, KeyStore(..))
import Data.String as DS

mobileNumberButtonConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
mobileNumberButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = (getString CONTINUE) 
                  ,  color = state.data.config.primaryTextColor 
                  , accessibilityHint = if state.props.btnActiveMobileNumber then "Continue : Button" else "Enter a valid Mobile Number to continue"
                  }
      , id = "PrimaryButtonMobileNumber"
      , isClickable = state.props.btnActiveMobileNumber
      , alpha = if state.props.btnActiveMobileNumber then 1.0 else 0.4
      , margin = (Margin 0 0 0 0 )
      , background = state.data.config.primaryBackground
      , enableLoader = (JB.getBtnLoader "PrimaryButtonMobileNumber")
      }
  in primaryButtonConfig'

whatsAppOTPButtonConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
whatsAppOTPButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { 
        text = (getString GET_OTP_VIA_WHATSAPP) 
      , color = state.data.config.driverInfoConfig.ratingTextColor 
        }
      , id = "PrimaryButtonWhatsAppOTP"
      , isClickable = state.props.btnActiveMobileNumber 
      , alpha = if state.props.btnActiveMobileNumber  then 1.0 else 0.4
      , margin = (Margin 0 0 0 0 )
      , background = Color.aliceBlue
      , enableLoader = (JB.getBtnLoader "PrimaryButtonMobileNumber")
      , stroke = ("1," <> Color.borderColorLight)
      , isSuffixImage = true
      , suffixImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_whatsapp_logo",
        margin = MarginLeft 10,
        gravity = CENTER
        }
      }
  in primaryButtonConfig'

verifyOTPButtonConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
verifyOTPButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CONTINUE) 
                  , color = state.data.config.primaryTextColor
                  , accessibilityHint = if state.props.btnActiveOTP then "Continue : Button" else "Enter a valid OTP to continue" }
      , id = "PrimaryButtonOTP"
      , isClickable = state.props.btnActiveOTP 
      , alpha = if state.props.btnActiveOTP  then 1.0 else 0.4
      , margin = (Margin 0 0 0 0 )
      , background = state.data.config.primaryBackground
      , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
      }
  in primaryButtonConfig'

mobileNumberEditTextConfig :: ST.EnterMobileNumberScreenState -> MobileNumberEditor.Config
mobileNumberEditTextConfig state = let 
    config = MobileNumberEditor.config
    mobileNumberEditor' = config
      { editText
        {
            color = Color.black800
          , singleLine = true
          , pattern = Just ("[0-9]*," <> show (mobileNumberMaxLength state.data.countryObj.countryShortCode))
          , margin = MarginHorizontal 10 10
          , focused = state.props.mNumberEdtFocused
          , text = state.props.editTextVal
          , accessibilityHint = (DS.replaceAll (DS.Pattern "") (DS.Replacement " ") state.data.mobileNumber)
          , placeholder = (getString ENTER_MOBILE_NUMBER)
        }
      , background = Color.white900
      , topLabel
        { text = (getString ENTER_YOUR_MOBILE_NUMBER)
        , color = Color.black800
        , alpha = 0.8
        , accessibility = DISABLE
        }
      , id = (EHC.getNewIDWithTag "EnterMobileNumberEditText")
      , type = "number"
      , height = V 54
      , margin = MarginTop 30
      , showErrorLabel = (not state.props.isValidMobileNumber)
      , errorLabel
        { text = (getString INVALID_MOBILE_NUMBER)
        , margin = MarginBottom 1
        }
      , showCountryCodeField= true
      , countryCodeField { 
          color = if state.props.countryCodeOptionExpanded then Color.black800 else Color.grey900 
        , padding = PaddingBottom 1
        , textStyle = FontStyle.SubHeading2
        , countryCodeOptionExpanded = state.props.countryCodeOptionExpanded
        , countryCode = state.data.countryObj.countryCode
        }
      }
    in mobileNumberEditor'

otpEditTextConfig :: ST.EnterMobileNumberScreenState -> PrimaryEditText.Config
otpEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , placeholder = (getString ENTER_4_DIGIT_OTP)
        , singleLine = true
        , pattern = Just "[0-9]*,4"
        , margin = MarginHorizontal 10 10
        , textStyle = FontStyle.Body7
        , letterSpacing = state.props.letterSpacing
        , text = ""
        , focused = state.props.otpEdtFocused
        , gravity = LEFT
        }
      , background = Color.white900
      , margin = (Margin 0 30 0 20)
      , topLabel
        { text = (getString LOGIN_USING_THE_OTP_SENT_TO) <> " " <> state.data.countryObj.countryCode <> " " <> state.data.mobileNumber
        , color = Color.black800
        , alpha = 0.8
        , accessibility = DISABLE
        } 
      , id = (EHC.getNewIDWithTag "EnterOTPNumberEditText")
      , type = "number"
      , height = V 54
      , errorLabel {
            text = if state.props.attemptLeft == "" then getString WRONG_OTP else getString INCORRECT_OTP_PLEASE_TRY_AGAIN <> state.props.attemptLeft <> getString N_MORE_ATTEMPTS_LEFT,
            visibility = VISIBLE,
            alpha = 0.8
      },
      width = MATCH_PARENT,
      showErrorLabel = state.props.wrongOTP, 
      stroke = if state.props.wrongOTP then ("1," <> Color.warningRed) else ("1," <> Color.borderColorLight)
      }
    in primaryEditTextConfig'

genericHeaderConfig :: ST.EnterMobileNumberScreenState ->  GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      , visibility = if state.props.enterOTP then VISIBLE else GONE-- config.prefixImageConfig.visibility -- Removed choose langauge screen
      } 
    -- , padding = (Padding 0 10 10 10)  
    }
  in genericHeaderConfig'
  










  
