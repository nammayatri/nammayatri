{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverDetailsScreen.ComponentConfig where

import Prelude (Unit, bind, const, map, pure, unit, ($), (/), (==), (<>),(<<<),(&&),(||),(<),not,(/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, afterRender, id, visibility, imageWithFallback, clickable, relativeLayout)
import Effect (Effect)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Language.Strings (getString)
import Language.Types(STR(..))
import Common.Types.App
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Components.InAppKeyboardModal.View as InAppKeyboardModal
import Components.InAppKeyboardModal.Controller as InAppKeyboardModalController
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (length)
import Components.PopUpModal.View as PopUpModal
import Components.PopUpModal.Controller as PopUpModalConfig
import PrestoDOM.Types.DomAttributes (Corners(..))
import EN

removeAlternateNumberConfig :: ST.DriverDetailsScreenState -> PopUpModalConfig.Config
removeAlternateNumberConfig state = let
    config = PopUpModalConfig.config
    popUpConfig' = config {
      gravity = BOTTOM,
      primaryText {
        text = (getString REMOVE_ALTERNATE_NUMBER)
      , margin = (Margin 16 24 16 0)
      },
      secondaryText {
        text = (getString ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER)
      , color = Color.black700
      , margin = (Margin 16 12 16 40)
        },
      option1 {
        text = (getString CANCEL)
      , fontSize = FontSize.a_16
      , color = Color.black900
      , strokeColor = Color.black700
      , fontStyle = FontStyle.semiBold LanguageStyle
      , testIdText = (getEN CANCEL)
      },
      option2 {text = (getString YES_REMOVE_IT)
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , fontSize = FontSize.a_16
      , margin = (MarginLeft 12)
      , fontStyle = FontStyle.semiBold LanguageStyle 
      , testIdText = (getEN YES_REMOVE_IT)
      }
    }
  in popUpConfig'

enterOtpExceededModalStateConfig:: ST.DriverDetailsScreenState -> PopUpModalConfig.Config
enterOtpExceededModalStateConfig state = let
    config = PopUpModalConfig.config
    popUpConf' = config {
      cornerRadius = (Corners 15.0 true true true true),
      margin = (Margin 16 290 16 200) ,
      gravity = CENTER
    ,primaryText {
        text = (getString OTP_LIMIT_EXCEEDED)
      , margin = (Margin 24 24 24 12)
      , visibility = VISIBLE
     },
      secondaryText {
        text = (getString OTP_LIMIT_EXCEEDED_MESSAGE)
      , color = Color.black600
      , margin = (Margin 24 0 24 32)
      , visibility = VISIBLE
        },
      option1 {
        text = (getString TRY_AGAIN_LATER)
      , fontSize = FontSize.a_16
      , color = Color.black900
      , strokeColor = Color.black700
      , fontStyle = FontStyle.semiBold LanguageStyle
      , visibility =false
      , testIdText = (getEN YES_REMOVE_IT)
      },
      option2 {text = (getString TRY_AGAIN_LATER)
      , color = Color.yellow900
      , strokeColor = Color.white900
      , fontSize = FontSize.a_16
      , margin = (Margin 16 0 16 0 )
      , fontStyle = FontStyle.semiBold LanguageStyle
      , width = (V 50)
      , testIdText = (getEN TRY_AGAIN_LATER)
      , background = Color.black900
      }
    }
  in popUpConf'

enterOtpState :: ST.DriverDetailsScreenState -> InAppKeyboardModalController.InAppKeyboardModalState
enterOtpState state = let
      config' = InAppKeyboardModalController.config
      inAppModalConfig' = config'{
        modalType = (if state.props.otpAttemptsExceeded then ST.NONE else ST.OTP),
        showResendOtpButton = true ,
        otpIncorrect = if (state.props.otpAttemptsExceeded) then false else (state.props.otpIncorrect),
        otpAttemptsExceeded = (state.props.otpAttemptsExceeded),
      inputTextConfig {
        text = state.props.alternateMobileOtp,
        fontSize = FontSize.a_22,
        focusIndex = state.props.enterOtpFocusIndex
      },
      headingConfig {
        text = getString (ENTER_OTP)
      },
      errorConfig {
        text = if (state.props.otpIncorrect) then ((getString WRONG_OTP) ) else ""
      , visibility = if (state.props.otpIncorrect || state.props.otpAttemptsExceeded) then VISIBLE else GONE,
      margin = (Margin 0 0 0 8)
      },
      subHeadingConfig {
        text = if((getValueToLocalStore LANGUAGE_KEY) == "EN_US") then (getString (OTP_SENT_TO) <> (if (state.props.isEditAlternateMobile) then (fromMaybe "" state.data.driverEditAlternateMobile) else (fromMaybe "" state.data.driverAlternateMobile))) else ( (if (state.props.isEditAlternateMobile) then (fromMaybe "" state.data.driverEditAlternateMobile) else (fromMaybe "" state.data.driverAlternateMobile)) <> (getString OTP_SENT_TO)),
        color = Color.black800,
        fontSize = FontSize.a_14,
        margin = (Margin 0 0 0 8),
        visibility = (if (state.props.otpIncorrect) == false then VISIBLE else GONE)
      },
      imageConfig {
          alpha = if (length state.props.alternateMobileOtp < 4 || state.props.otpIncorrect) then 0.3 else 1.0
      }
      }
      in inAppModalConfig'

enterMobileNumberState :: ST.DriverDetailsScreenState -> InAppKeyboardModalController.InAppKeyboardModalState
enterMobileNumberState state = let
      config' = InAppKeyboardModalController.config
      inAppModalConfig' = config'{
        errorConfig {
          text = if state.props.numberExistError then (getString NUMBER_ALREADY_EXIST_ERROR) else (getString PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER)
        , visibility = if state.props.numberExistError then VISIBLE else (if (state.props.isEditAlternateMobile) then (if (state.data.driverEditAlternateMobile == Nothing || (state.props.checkAlternateNumber) ) then GONE else VISIBLE) else (if (state.data.driverAlternateMobile == Nothing || (state.props.checkAlternateNumber) ) then GONE else VISIBLE))
        , gravity = LEFT
        },
        headingConfig {
          text = if ( (state.props.isEditAlternateMobile == false)) then (getString ENTER_ALTERNATE_MOBILE_NUMBER) else (getString EDIT_ALTERNATE_MOBILE_NUMBER)
        },
        subHeadingConfig {
          visibility = GONE
        },
        inputTextConfig {
          text = if(state.props.isEditAlternateMobile) then (if (state.data.driverEditAlternateMobile  == Nothing) then (getString ENTER_MOBILE_NUMBER) else fromMaybe "" (state.data.driverEditAlternateMobile)) else (if (state.data.driverAlternateMobile == Nothing) then (getString ENTER_MOBILE_NUMBER) else fromMaybe "" (state.data.driverAlternateMobile))
        , color = if(state.props.isEditAlternateMobile) then (if( state.data.driverEditAlternateMobile == Nothing ) then Color.black500 else Color.black800) else (if( state.data.driverAlternateMobile == Nothing ) then Color.black500 else Color.black800)
        , fontSize = FontSize.a_16
        , focusIndex = 0
        , gravity = LEFT
        },
        imageConfig {
          alpha = case state.data.driverAlternateMobile of
                Nothing -> 0.3
                Just _ -> if (length (fromMaybe "" state.data.driverAlternateMobile) < 10 || state.props.checkAlternateNumber==false || (state.props.isEditAlternateMobile == true &&  length (fromMaybe "" state.data.driverEditAlternateMobile) < 10)|| state.props.numberExistError)
                then 0.3 else 1.0
        },
       modalType = (if state.props.otpAttemptsExceeded then ST.NONE else ST.MOBILE__NUMBER),
      isValidAlternateNumber = if state.props.numberExistError == true then false else state.props.checkAlternateNumber
        }
      in inAppModalConfig'
