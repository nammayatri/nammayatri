{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DocumentCaptureScreen.ComponentConfig where 

import Components.GenericHeader as GenericHeader 
import Components.PrimaryButton as PrimaryButton 
import Components.PrimaryEditText as PrimaryEditText
import Components.MobileNumberEditor as MobileNumberEditor
import PrestoDOM
import Screens.Types as ST 
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude
import Common.Types.App(LazyCheck(..))
import Font.Style as FontStyle
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Helpers.Utils as HU
import Components.ValidateDocumentModal as ValidateDocumentModal
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Resource.Constants as Constant
import Data.String as DS
import Data.Array as DA
import Data.Maybe (isJust, fromMaybe)
import Components.OptionsMenu as OptionsMenuConfig
import Storage (KeyStore(..), getValueToLocalStore)
import Components.BottomDrawerList as BottomDrawerList
import ConfigProvider
import Data.Maybe
import Engineering.Helpers.Commons as EHC
import JBridge as JB


primaryButtonConfig :: ST.DocumentCaptureScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    len = if state.props.isSSNView then DS.length state.data.ssn else 0
    isValid = len == 9
    primaryButtonConfig' = config 
      {   textConfig
        { text = if state.props.isSSNView then "Submit" else getString UPLOAD_PHOTO } 
        , margin = Margin 16 16 16 (EHC.safeMarginBottomWithDefault 16)
        , id = "DocCaptureButton"
        , isClickable = if state.props.isSSNView then if isValid  then true else false else true
        , alpha =  if state.props.isSSNView then if isValid then 1.0 else 0.4 else 1.0
        , enableLoader = if state.props.isSSNView then JB.getBtnLoader "DocCaptureButton" else false
        , lottieConfig {
          forceToUseRemote = true
        , autoDisableLoader = true
        }
      }
  in primaryButtonConfig'


profileViewPrimaryButtonConfig :: ST.DocumentCaptureScreenState -> PrimaryButton.Config
profileViewPrimaryButtonConfig state = let 
    config = PrimaryButton.config
    isValid = state.props.isValidFirstName && (state.props.isValidMobileNumber || state.props.isValidEmail) && isJust state.data.firstName && (isJust state.data.mobileNumber || isJust state.data.email)
    primaryButtonConfig' = config 
      {   textConfig
        { text = "Submit" } 
        , margin = Margin 16 16 16 (EHC.safeMarginBottomWithDefault 16)
        , id = "DocCaptureSubmitButton"
        , enableLoader = JB.getBtnLoader "DocCaptureSubmitButton"
        , isClickable = isValid
        , lottieConfig {
          forceToUseRemote = true
        }
        , alpha = if isValid then 1.0 else 0.4
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.DocumentCaptureScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  uiConfig = getAppConfig appConfig
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = uiConfig.secondaryBackground
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = HU.fetchImage HU.FF_ASSET "ic_new_avatar"
      , height = V 25
      , width = V 25
      , margin = Margin 12 5 5 5
      }
    , padding = PaddingVertical 5 5
    , textConfig {
        text = getValueToLocalStore MOBILE_NUMBER_KEY
      , color = Color.white900
      , margin = MarginHorizontal 5 5 
      , textStyle = FontStyle.Body1
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

appOnboardingNavBarConfig :: ST.DocumentCaptureScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { genericHeaderConfig = genericHeaderConfig state,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig{ 
      text = if state.props.isSSNView then "Social Security Number" else if state.props.isProfileView then "Profile Details" else getVarString UPLOAD_DOC [Constant.transformDocText state.data.docType]
      },
    rightButton = AppOnboardingNavBar.config.rightButton{
      text = getString HELP_FAQ
      }
  }

validateDocModalState :: ST.DocumentCaptureScreenState -> ValidateDocumentModal.ValidateDocumentModalState
validateDocModalState state = 
  ValidateDocumentModal.config {
    background = Color.black,
    profilePictureCapture = false,
    verificationStatus = if state.props.validating then ST.InProgress 
                          else if isJust state.data.errorMessage then ST.Failure
                          else if not DS.null state.data.docId then ST.Success
                          else ST.None,
    verificationType = "OTHER",
    failureReason = fromMaybe "" state.data.errorMessage,
    headerConfig {
      imageConfig {
      color = Color.white900
    },
      headTextConfig {
        text = getString TAKE_PHOTO,
        color = Color.white900
      }
    }
  }

optionsMenuConfig :: ST.DocumentCaptureScreenState -> OptionsMenuConfig.Config
optionsMenuConfig state = OptionsMenuConfig.config {
  menuItems = [
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_phone_unfilled", textdata : getString CONTACT_SUPPORT, action : "contact_support", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_language", textdata : getString CHANGE_LANGUAGE_STR, action : "change_language", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_parallel_arrows_horizontal", textdata : getString CHANGE_VEHICLE, action : "change_vehicle", isVisible : (DA.length state.data.variantList > 1), color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_logout_grey", textdata : getString LOGOUT, action : "logout", isVisible :  true, color : Color.black800}
  ],
  backgroundColor = Color.blackLessTrans,
  menuBackgroundColor = Color.white900,
  gravity = RIGHT,
  menuExpanded = true,
  width = WRAP_CONTENT,
  marginRight = 16,
  itemHeight = V 50,
  itemPadding = Padding 16 16 16 16,
  cornerRadius = 4.0,
  enableAnim = true
}

bottomDrawerListConfig :: ST.DocumentCaptureScreenState -> BottomDrawerList.Config
bottomDrawerListConfig state = BottomDrawerList.config {
  animState = state.props.contactSupportModal,
  titleText = getString CONTACT_SUPPORT_VIA,
  itemList = [
    {prefixImg : "ny_ic_whatsapp_black", title : "Whatsapp", desc : getString YOU_CAN_SHARE_SCREENSHOT , postFixImg : "ny_ic_chevron_right", visibility : state.data.cityConfig.registration.whatsappSupport, identifier : "whatsapp"},
    {prefixImg : "ny_ic_direct_call", title : getString CALL, desc : getString PLACE_A_CALL, postFixImg : "ny_ic_chevron_right", visibility : true, identifier : "call"},
    {prefixImg : "ny_ic_mail", title : "Mail", desc : "Write an email for any help in uploading the documents" , postFixImg : "ny_ic_chevron_right", visibility : state.data.cityConfig.registration.emailSupport, identifier : "email"}
  ]
}

ssnPrimaryEditTextConfig :: ST.DocumentCaptureScreenState -> PrimaryEditText.Config
ssnPrimaryEditTextConfig state = 
  PrimaryEditText.config
      { editText
        { color = Color.black800
        , textStyle = FontStyle.Body1
        , margin = (Margin 16 16 16 16)
        , placeholder = "000-00-0000"
        , pattern = Just "[0-9]*,9"
        }
      , background = Color.white900
      , topLabel
        { text = "Social Security Number (SSN)"
        , color = Color.black800
        , textStyle = FontStyle.Body3
        }  
      , margin = MarginTop 0
      , type = "number"
      }

firstNamePrimaryEditTextConfig :: ST.DocumentCaptureScreenState -> PrimaryEditText.Config
firstNamePrimaryEditTextConfig state = 
  PrimaryEditText.config { editText
        { color = Color.black800
        , placeholder = "Enter your First Name"
        , singleLine = true
        , pattern = Just "[a-zA-Z. ]*,30"
        , margin = MarginHorizontal 10 10
        , textStyle = FontStyle.Body7
        , focused = true
        , gravity = LEFT
        }
      , background = Color.white900
      , margin = MarginVertical 16 16
      , topLabel
        { text = "First Name"
        , color = Color.black800
        , alpha = 0.8
        } 
      , id = (EHC.getNewIDWithTag "FirstNameEditText")
      , height = V 54
      , errorLabel {
            text = "Name should be more than 2 characters",
            visibility = VISIBLE,
            alpha = 0.8
      },
      focusedStroke = "1," <> Color.borderColorLight,
      width = MATCH_PARENT,
      showErrorLabel = not state.props.isValidFirstName, 
      stroke = if not state.props.isValidFirstName then ("1," <> Color.warningRed) else ("1," <> Color.borderColorLight)
      }
lastNamePrimaryEditTextConfig :: ST.DocumentCaptureScreenState -> PrimaryEditText.Config
lastNamePrimaryEditTextConfig state = 
  PrimaryEditText.config { editText
        { color = Color.black800
        , placeholder = "Enter your Last Name"
        , singleLine = true
        , pattern = Just "[a-zA-Z. ]*,30"
        , margin = MarginHorizontal 10 10
        , textStyle = FontStyle.Body7
        , gravity = LEFT
        }
      , background = Color.white900
      , margin = MarginVertical 16 16
      , topLabel
        { text = "Last Name"
        , color = Color.black800
        , alpha = 0.8
        } 
      , id = (EHC.getNewIDWithTag "LastNameEditText")
      , height = V 54
      , errorLabel {
            visibility = GONE
      },
      focusedStroke = "1," <> Color.borderColorLight,
      width = MATCH_PARENT 
      }

mobileNumberPrimaryEditTextConfig :: ST.DocumentCaptureScreenState -> PrimaryEditText.Config
mobileNumberPrimaryEditTextConfig state = 
  let mobileNumber = fromMaybe "" state.data.mobileNumber 
  in
  PrimaryEditText.config
      { editText
        { color = Color.black800
        , placeholder = "Enter your Mobile Number"
        , singleLine = true
        , margin = MarginHorizontal 10 10
        , pattern = Just "[0-9]*,10"
        , textStyle = FontStyle.Body7
        , gravity = LEFT
        }
      , background = Color.white900
      , showErrorLabel = mobileNumber /= "" && not state.props.isValidMobileNumber
      , id = (EHC.getNewIDWithTag "MobileNumberEditText")
      , topLabel
        { text = "Mobile Number"
        , color = Color.black800
        , alpha = 0.8
        }  
      , type = "number"
      , margin = MarginVertical 16 16
      , errorLabel{
          text = "Enter a Valid Mobile Number"
        , color = Color.textDanger
        }
      }

emailIdPrimaryEditTextConfig :: ST.DocumentCaptureScreenState -> PrimaryEditText.Config
emailIdPrimaryEditTextConfig state = 
  let email = fromMaybe "" state.data.email
  in
  PrimaryEditText.config
      { editText
        { color = Color.black800
        , placeholder = "Enter your Email id"
        , singleLine = true
        , margin = MarginHorizontal 10 10
        , textStyle = FontStyle.Body7
        , gravity = LEFT
        }
      , background = Color.white900
      , showErrorLabel = email /= "" && not state.props.isValidEmail
      , id = (EHC.getNewIDWithTag "EmailIdPrimaryEditTextBox")
      , topLabel
        { text = "Email ID"
        , color = Color.black800
        , alpha = 0.8
        }  
      , margin = MarginVertical 16 16
      , errorLabel{
          text = "Enter a Valid Email Id"
        , color = Color.textDanger
        }
      }


mobileNumberConfig :: ST.DocumentCaptureScreenState -> MobileNumberEditor.Config
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
      { text = "Mobile Number"
      , color = Color.black800
      , accessibility = DISABLE
      , textStyle = FontStyle.SubHeading1
      }
    , type = "number"
    , id = (EHC.getNewIDWithTag "MobileNumberEditText")
    , errorLabel
        { text = (getString INVALID_MOBILE_NUMBER)
        , margin = MarginBottom 1
        }
    , showErrorLabel = not state.props.isValidMobileNumber
    , countryCodeOptionConfig {
        padding = Padding 16 ((if EHC.os == "IOS" then 16 else 12)) 8 (if EHC.os == "IOS" then 16 else 12)
    }
    }
  in mobileNumberEditor'
