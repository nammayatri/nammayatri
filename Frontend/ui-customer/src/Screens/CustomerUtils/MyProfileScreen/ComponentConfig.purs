{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.MyProfileScreen.ComponentConfig where

import Screens.Types as ST 
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (not, negate)
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..))
import Styles.Colors as Color
import Animation.Config (AnimConfig, animConfig)
import PrestoDOM.Animation as PrestoAnim
import Common.Types.App

genericHeaderConfig :: ST.MyProfileScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , prefixImageConfig {
        height = (V 35)
      , width = (V 35)
      , margin = (Margin 10 17 16 15)
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/coomon/ny_ic_chevron_left.png"
      , padding = (Padding 5 5 5 5 )
      }
    , textConfig {
        text = if state.props.updateProfile then (getString UPDATE_PERSONAL_DETAILS) else (getString PERSONAL_DETAILS)
      , textSize = FontSize.a_18
      , color = Color.black
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    }
  in genericHeaderConfig'
  
nameEditTextConfig :: ST.MyProfileScreenState -> PrimaryEditText.Config
nameEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
        {
            margin = (Margin 16 32 16 0),
            topLabel {
                text = (getString NAME),
                textSize = FontSize.a_12,
                color = Color.black900,
                fontStyle = FontStyle.regular LanguageStyle
            },
            editText {
                text = state.data.name,
                textSize = FontSize.a_16,
                fontStyle = FontStyle.semiBold LanguageStyle,
                pattern = Just "[a-zA-Z ]*,30"
            },
            id = (EHC.getNewIDWithTag "UserNameEditText")
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
                textSize = FontSize.a_12,
                color = Color.black900,
                fontStyle = FontStyle.regular LanguageStyle
            },
            editText {
                text = fromMaybe "" state.data.emailId,
                placeholder = "example@xyz.com",
                placeholderColor = Color.black600,
                textSize = FontSize.a_16,
                fontStyle = FontStyle.semiBold LanguageStyle
            },
            errorLabel{
              text = case state.data.errorMessage of 
                Just ST.EMAIL_EXISTS -> "Email already exists"
                Just ST.INVALID_EMAIL -> "Please enter a valid email"
                Nothing -> ""
            , fontStyle = FontStyle.regular LanguageStyle
            , color = Color.textDanger
            },
            id = (EHC.getNewIDWithTag "EmailEditText")
        }
    in primaryEditTextConfig'

updateButtonConfig :: ST.MyProfileScreenState -> PrimaryButton.Config
updateButtonConfig state = let
    config = PrimaryButton.config
    updateButtonConfig' = config 
        { textConfig{ text = (getString UPDATE), color = state.data.config.primaryTextColor}
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

requestDeletePopUp :: ST.MyProfileScreenState -> PopUpModal.Config 
requestDeletePopUp state = let 
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText { text =(getString DEL_ACCOUNT)},
      secondaryText { text = (getString ACCOUNT_DELETION_CONFIRMATION),
      padding = (Padding 36 0 36 0),
      color = Color.black600},
      option1 {
        text = (getString CANCEL_STR)
      , fontSize = FontSize.a_16
      },
      option2 {text = (getString YES_DELETE_IT)
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , fontSize = FontSize.a_16 }
     
    }
  in popUpConfig'

accountDeletedPopUp :: ST.MyProfileScreenState -> PopUpModal.Config 
accountDeletedPopUp state = let 
    config = PopUpModal.config 
    popUpConfig' = config {
      primaryText{ text= (getString REQUEST_SUBMITTED)},
      secondaryText{text = (getString WE_WILL_DELETE_YOUR_ACCOUNT),
      padding = (Padding 16 0 16 0),
      color = Color.black600},
      option1 {
        visibility = false
      },
      option2 {
        visibility = false
      }
    }
    in popUpConfig'

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