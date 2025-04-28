{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.InAppKeyboardModal.Controller where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Prelude ((<>), class Eq)
import Font.Size as FontSize
import Font.Style (Style (..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), height, width)
import Styles.Colors as Color
import Common.Types.App(LazyCheck(..))
import Screens.Types(KeyboardModalType(..))
import Components.PrimaryButton.Controller as PrimaryButton
import MerchantConfig.Types (AppConfig)
import ConfigProvider

data Action = OnSelection String Int
            | OnClickDone String
            | AfterRender
            | OnClickBack String
            | OnclickTextBox Int
            | BackPressed
            | OnClickResendOtp
            | OnClickTextCross
            | NoAction
            | OnTextViewClick String 
            | RetakeParcelImage
            | PrimaryButtonAction PrimaryButton.Action

----------------------------------------------- InAppKeyboardModalState ---------------------------------------------
type InAppKeyboardModalState = {
      appConfig :: AppConfig
    , errorConfig :: TextConfig
    , headingConfig :: TextConfig
    , subHeadingConfig :: TextConfig
    , inputTextConfig :: TextConfig
    , bodyTextConfig :: TextConfig
    , buttonConfig :: ButtonConfig
    , imageConfig :: ImageConfig
    , keyList :: Array Keys
    , otpIncorrect :: Boolean
    , otpAttemptsExceeded :: Boolean
    , modalType :: KeyboardModalType
    , isValidAlternateNumber :: Boolean
    , showResendOtpButton :: Boolean
    , textBoxConfig :: TextBoxConfig
    , enableDeviceKeyboard :: Boolean
    , confirmBtnColor :: String
    , isDismissable :: Boolean
    , showRetakeParcelImage :: Boolean
    , primaryButtonConfig :: PrimaryButton.Config
}

type TextBoxConfig = {
  textBoxesArray :: Array Int
  , width :: Length
  , height :: Length
  , margin :: Margin
}

type TextConfig =
  { text :: String
    , focusIndex :: Int
    , gravity :: Gravity
    , visibility :: Visibility
    , color :: String
    , height :: Length
    , width :: Length
    , cornerRadius :: Number
    , padding :: Padding
    , margin :: Margin
    , weight :: Number
    , textStyle :: Style
    , background :: String
    , suffixImageVisibility :: Boolean
    , strokeColor :: String
  }

type ImageConfig =
  {
    imageUrl :: String
  , height :: Length
  , width :: Length
  , margin :: Margin
  , visibility :: Visibility
  , alpha :: Number
  }

type ButtonConfig =
  { margin :: Margin
  , text :: String
  , color :: String
  , width :: Length
  , height :: Length
  , cornerRadius :: Number
  , stroke :: String
  , background :: String
  , visibility :: Visibility
  , textStyle :: Style
  }

type Keys = {
  keys :: Array String
}

type SingleElementTextBoxConfig = {
  height :: Length,
  width :: Length,
  margin :: Margin,
  numberOfBoxes :: Int
  }


type InputFieldConfig = 
  { isAdjustable :: Boolean,
    textVal :: String ,
    letterSpacing :: Number,
    width :: Length,
    isActive :: Boolean,
    unitVal :: String
  }

config :: InAppKeyboardModalState
config = {
    appConfig : getAppConfig appConfig,
    errorConfig : {
      text : ""
    , focusIndex : 0
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.red
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 0.0
    , textStyle : Body1
    , background : Color.transparent
    , suffixImageVisibility : false
    , strokeColor : "1," <> Color.borderColorLight
    },
    headingConfig : {
      text : ""
    , focusIndex : 0
    , textStyle : Heading3
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black800
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (MarginLeft 16)
    , weight : 0.0
    , background : Color.transparent
    , suffixImageVisibility : false
    , strokeColor : "1," <> Color.borderColorLight
    },
    subHeadingConfig : {
      text : ""
    , focusIndex : 0
    , gravity : CENTER
    , textStyle : Tags
    , visibility : VISIBLE
    , color : Color.black900
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 500.0
    , background : Color.transparent
    , suffixImageVisibility : false
    , strokeColor : "1," <> Color.borderColorLight
    },
    inputTextConfig : {
       text : ""
    , focusIndex : 1
    , textStyle : SubHeading1
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black800
    , height : WRAP_CONTENT
    , width : MATCH_PARENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 1.0
    , background : Color.transparent
    , suffixImageVisibility : true
    , strokeColor : "1," <> Color.borderColorLight
    },
    bodyTextConfig : {
       text : ""
    , focusIndex : 1
    , textStyle : Body1
    , gravity : CENTER
    , visibility : GONE
    , color : Color.black800
    , height : WRAP_CONTENT
    , width : MATCH_PARENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 1.0
    , background : Color.transparent
    , suffixImageVisibility : false
    , strokeColor : "1," <> Color.borderColorLight
    },
    buttonConfig : {
      margin : (Margin 0 0 0 0)
    , text : ""
    , textStyle : Heading2
    , color : Color.yellow900
    , width : MATCH_PARENT
    , height : MATCH_PARENT
    , cornerRadius : 8.0
    , stroke : "0,#ffffff"
    , background : Color.black
    , visibility : VISIBLE
    },
    imageConfig : {
      imageUrl : ""
    , height : V 124
    , width : V 124
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    , alpha : 1.0
    },
    keyList : [
    {
        keys: ["1", "2", "3"]
    },
    {
        keys: ["4", "5", "6"]
    },
    {
        keys: ["7", "8", "9"]
    },
    {
        keys: ["back", "0", "done"]
    }
    ]
  , otpIncorrect : false
  , otpAttemptsExceeded : false
  , modalType : NONE
  , isValidAlternateNumber : true
  , showResendOtpButton : false
  , textBoxConfig : {
      textBoxesArray : [1,2,3,4],
      width : V 48,
      height : V 56,
      margin : (Margin 0 0 0 0)
  }
  , isDismissable : true
  , confirmBtnColor : Color.darkMint
  , enableDeviceKeyboard : false
  , showRetakeParcelImage : false
  , primaryButtonConfig : PrimaryButton.config {visibility = GONE}
  }


primaryButtonConfig :: InAppKeyboardModalState -> PrimaryButton.Config
primaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = state.primaryButtonConfig.textConfig.text
      , color = state.primaryButtonConfig.textConfig.color
      , buttonInactiveTextColor = state.primaryButtonConfig.textConfig.buttonInactiveTextColor
      }
      , background = state.primaryButtonConfig.background
      , buttonInactiveBackground = state.primaryButtonConfig.buttonInactiveBackground
      , visibility = state.primaryButtonConfig.visibility
      , isClickable = state.primaryButtonConfig.isClickable
      }
  in primaryButtonConfig'
