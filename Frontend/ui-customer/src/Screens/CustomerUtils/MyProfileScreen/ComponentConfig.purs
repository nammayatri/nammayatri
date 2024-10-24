{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.MyProfileScreen.ComponentConfig where

import Common.Types.App

import Animation.Config (AnimConfig, animConfig)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.GenericRadioButton as GenericRadioButton
import Components.SelectListModal as SelectListModal
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude
import Data.String as DS
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style (Style(..))
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..),isParentView, showTitle, validateEmail)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>))
import Prelude (not, negate)
import PrestoDOM (Length(..), Margin(..), Padding(..), Gravity(..), Visibility(..))
import PrestoDOM.Animation as PrestoAnim
import Screens.Types as ST
import Styles.Colors as Color
import Data.Array as DA
import Components.PopUpModal as PopUpModal
import Screens.MyProfileScreen.Controller (isEmailPresent)
import Storage (getValueToLocalStore, KeyStore(..))

genericHeaderConfig :: ST.MyProfileScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = if state.data.config.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  btnVisibility = if isParentView FunctionCall then GONE else config.prefixImageConfig.visibility
  titleVisibility = if state.props.updateProfile || showTitle FunctionCall then config.visibility else GONE
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , prefixImageConfig {
        height = (V 35)
      , width = (V 35)
      , margin = (Margin 10 17 16 15)
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , padding = (Padding 5 5 5 5 )
      , visibility =  btnVisibility
      }
    , textConfig {
        text = if state.props.updateProfile then (getString UPDATE_PERSONAL_DETAILS) else (getString PERSONAL_DETAILS)
      , color = Color.black
      }
    , visibility = titleVisibility
    }
  in genericHeaderConfig'
  
nameEditTextConfig :: ST.MyProfileScreenState -> PrimaryEditText.Config
nameEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
        {
            margin = (Margin 16 32 16 0),
            showErrorLabel = (not state.props.isNameValid),
            topLabel {
                text = (getString NAME),
                color = Color.black900
            },
            editText {
                text = state.data.name,
                pattern = Just "[a-zA-Z. ]*,30"
              , textStyle = SubHeading1
            },
            errorLabel{
              text = case state.data.nameErrorMessage of 
                Just ST.INVALID_NAME -> getString NAME_SHOULD_BE_MORE_THAN_2_CHARACTERS
                Just ST.NAME_CANNOT_BE_BLANK -> getString THIS_FIELD_IS_REQUIRED
                _ -> ""
            , color = Color.textDanger
            }
            , id = (EHC.getNewIDWithTag "UserNameEditText")
            , width = MATCH_PARENT
        }
    in primaryEditTextConfig'

emailEditTextConfig :: ST.MyProfileScreenState -> PrimaryEditText.Config
emailEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
        {
            margin = (Margin 16 32 16 0),
            showErrorLabel = (not state.props.isEmailValid),
            topLabel {
                text = (getString EMAIL_ID),
                color = Color.black900
            },
            editText {
                text = fromMaybe "" state.data.emailId,
                placeholder = "example@xyz.com",
                placeholderColor = Color.black600
            },
            errorLabel{
              text = case state.data.emailErrorMessage of 
                Just ST.EMAIL_EXISTS -> getString EMAIL_EXISTS_ALREADY
                Just ST.INVALID_EMAIL -> getString PLEASE_ENTER_A_VALID_EMAIL
                Just ST.EMAIL_CANNOT_BE_BLANK -> getString THIS_FIELD_IS_REQUIRED
                _ -> ""
            , color = Color.textDanger
            }
            , id = (EHC.getNewIDWithTag "EmailEditText")
            , width = MATCH_PARENT

        }
    in primaryEditTextConfig'

updateButtonConfig :: ST.MyProfileScreenState -> PrimaryButton.Config
updateButtonConfig state = let
    config = PrimaryButton.config
    updateButtonConfig' = config 
        { textConfig{ text = (getString UPDATE), color = state.data.config.primaryTextColor
        , accessibilityHint = if state.props.isBtnEnabled then "Update Personal Details Button " else if not state.props.isEmailValid then "Update Button Is DisAbled : Please Enter A Valid Email" else if (not state.props.isNameValid) then  "Update Button Is DisAbled : Please Enter A Valid Name" else "Update Button Is DisAbled"}
        , height = (V 48)
        , cornerRadius = 8.0
        , margin = (Margin 16 0 16 0)
        , id = "PrimaryButtonUpdate"
        , enableLoader = (JB.getBtnLoader "PrimaryButtonUpdate")
        , isClickable = state.props.isBtnEnabled
        , alpha = if state.props.isBtnEnabled then 1.0 else 0.5
        , background = state.data.config.primaryBackground
        }
    in updateButtonConfig'

listExpandingAnimationConfig :: ST.MyProfileScreenState -> AnimConfig
listExpandingAnimationConfig state = let 
  config = getConfig state 
  animConfig' = animConfig 
          { fromScaleY = config.fromScaleY
          , toScaleY = config.toScaleY
          , fromY = config.fromY
          , toY = config.toY
          , repeatCount = (PrestoAnim.Repeat 0)
          , ifAnim = state.props.expandEnabled
          , duration = 200
          } 
  in animConfig'

getConfig :: forall w. ST.MyProfileScreenState -> {fromScaleY :: Number , toScaleY :: Number, fromY :: Int, toY :: Int}
getConfig state = 
  if state.props.genderOptionExpanded then 
    { fromScaleY : 0.0
    , toScaleY : 1.0
    , fromY : -100
    , toY : 0
    } 
  else  
    { fromScaleY : 1.0
    , toScaleY : 0.0
    , fromY : 0
    , toY : -100
    }

getRadioButtonConfig :: Int -> String -> ST.MyProfileScreenState -> GenericRadioButton.Config
getRadioButtonConfig index item state = GenericRadioButton.config {
  activeButtonConfig {
    stroke = Color.blue900
  , buttonColor = Color.black800
  , background = Color.blue600
  },
  buttonTextConfig {
    text = item
  , color = Color.black900
  },
  editTextConfig {
    hint = "Enter nature of condition"
  }
  , inActiveButtonConfig {
    stroke = Color.grey900
  , buttonColor = Color.black600
  , background = Color.white900
  }
  , isSelected = index == state.data.editedDisabilityOptions.activeIndex
  , id = index
}

deleteAccountButtonConfig :: ST.MyProfileScreenState -> PrimaryButton.Config
deleteAccountButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        {
          textFromHtml = Just $ "<u>" <> (getString DEL_ACCOUNT) <> "</u>"
        , color = Color.black700
        , id = "deleteAccountButtonTextConfig"
        , textStyle = FontStyle.ParagraphText
        }
      , height = V 20
      , width = WRAP_CONTENT
      , isClickable  = true
      , cornerRadius = 0.0
      , margin = Margin 0 0 0 0
      , id = "deleteAccountButtonConfig"
      , padding = Padding 20 0 20 0
      , background = Color.white900
      }
  in primaryButtonConfig'

requestDeletePopUp :: ST.MyProfileScreenState -> PopUpModal.Config
requestDeletePopUp state = let
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText { text = getString DEL_ACCOUNT },
      secondaryText { text = getString ACCOUNT_DELETION_CONFIRMATION,
      padding = PaddingHorizontal 36 36,
      color = Color.black600},
      option1 {
        text = getString CANCEL_STR
      },
      option2 {text = getString YES_DELETE_IT
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red }
    }
  in popUpConfig'

accountDeletedPopUp :: ST.MyProfileScreenState -> PopUpModal.Config
accountDeletedPopUp state = let
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText{ text = getString REQUEST_SUBMITTED},
      secondaryText{text = getString WE_WILL_DELETE_YOUR_ACCOUNT,
      padding = PaddingHorizontal 16 16,
      color = Color.black600},
      option1 {
        visibility = false
      },
      option2 {
        text = getString OKAY_GOT_IT,
        margin = MarginHorizontal 16 16
      }
    }
    in popUpConfig'

deleteGenericHeaderConfig :: ST.MyProfileScreenState -> GenericHeader.Config
deleteGenericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = Margin 8 8 8 8
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      }
    , padding = PaddingVertical 5 5
    , textConfig {
        text = getString DEL_ACCOUNT
      , color = Color.black900
      }
    }
  in genericHeaderConfig'

primaryEditTextConfigEmail :: ST.MyProfileScreenState -> PrimaryEditText.Config
primaryEditTextConfigEmail state = 
  let email = state.data.delAccEmail
      config = PrimaryEditText.config
      primaryEditTextConfig' = config
        { editText
          { color = if isEmailPresent FunctionCall then Color.black600 else Color.black800
          , margin = Margin 16 16 16 16
          , placeholder = "example@xyz.com"
          , text = if isEmailPresent FunctionCall then getValueToLocalStore USER_EMAIL else ""
          , enabled = not isEmailPresent FunctionCall
          , textStyle = FontStyle.Body1
          }
        , background = Color.white900
        , topLabel
          { text = getString YOUR_EMAIL_ID <> "*"
          , color = Color.black900
          }
        , showErrorLabel = not validateEmail email && DS.length email > 0
        , errorLabel
          { text = getString PLEASE_ENTER_A_VALID_EMAIL
          , color = Color.textDanger }
        , margin = Margin 10 32 10 0
        }
  in primaryEditTextConfig'

primaryEditTextConfigDescription :: ST.MyProfileScreenState -> PrimaryEditText.Config
primaryEditTextConfigDescription state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , margin = if EHC.os == "IOS" then Margin 10 16 10 10 else Margin 16 16 16 16
        , singleLine = false
        , placeholder = getString YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE
        , pattern = Just "[A-Za-z0-9,. ]*,300"
        , textStyle = FontStyle.Body1
        }
      , background = Color.white900
      , height = V 120
      , stroke = "1," <> if DS.length state.data.description >= 300 then Color.textDanger else Color.borderColorLight
      , topLabel
        { text = getString REASON_FOR_DELETING_ACCOUNT
        , color = Color.black900
        }
      , margin = Margin 10 32 10 0
      , showErrorLabel = DS.length state.data.description >= 300
      , errorLabel
        { text = getString MAX_CHAR_LIMIT_REACHED <> " 300 " <> getString OF <> " 300"
        , color = Color.textDanger
        }
      }
    in primaryEditTextConfig'

primaryButtonConfigSubmitRequest :: ST.MyProfileScreenState -> PrimaryButton.Config
primaryButtonConfigSubmitRequest state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = getString SUBMIT_REQUEST
        , accessibilityHint = if state.props.btnActive then "Submit Request : Button" else "Submit Request Button Disabled"
        , color = if state.props.btnActive then Color.yellowRadler else Color.yellow800
        }
      , cornerRadius = 8.0
      , background = if state.props.btnActive then Color.black900 else Color.black500
      , isClickable = state.props.btnActive
      , margin = Margin 16 0 16 (if EHC.safeMarginBottom == 0 then 24 else (EHC.safeMarginBottom))
      , id = "ButtonDeleteAccount"
      }
  in primaryButtonConfig'