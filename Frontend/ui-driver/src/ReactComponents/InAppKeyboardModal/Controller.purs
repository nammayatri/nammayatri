{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.InAppKeyboardModal.Controller where

import Common.Types.App (LazyCheck(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style (Style(..))
import Prelude (Unit, pure, unit, (<>), (==), (>), (-))
import PrestoDOM (Gravity(..), Length(..), Padding(..), Visibility(..))
import Screens.Types (KeyboardModalType(..))
import Styles.Colors as Color
import Data.String (length, take)

data ComponentOutput = OnClickDone String | BackPressed | OnClickResendOtp

data ComponentAction = OnSelection String Int
            | AfterRender
            | OnClickBack String
            | OnclickTextBox Int
            | OnClickTextCross

reducerFn :: InAppKeyboardModalState -> ComponentAction -> InAppKeyboardModalState
reducerFn state action =
  let _ = spy "InAppKeyboardModal Component Action" action
  in case action of
    OnSelection key index -> state { inputTextConfig { text = (state.inputTextConfig.text <> key), focusIndex = index } }
    -- AfterRender -> setState (\state -> state { inputTextConfig = (state.inputTextConfig state) { focusIndex = 1 } })
    OnClickBack value -> do
      let 
        text = (if length( value ) > 0 then (take (length ( value ) - 1 ) value) else "" )
        focusIndex = length text
      state { inputTextConfig { text = text, focusIndex = focusIndex } }
    OnclickTextBox index -> state{inputTextConfig{ focusIndex = index}}
    _ -> state

----------------------------------------------- InAppKeyboardModalState ---------------------------------------------
type InAppKeyboardModalState = {
      errorConfig :: TextConfig
    , headingConfig :: TextConfig
    , subHeadingConfig :: TextConfig
    , inputTextConfig :: TextConfig
    , buttonConfig :: ButtonConfig
    , imageConfig :: ImageConfig
    , keyList :: Array Keys
    , otpIncorrect :: Boolean
    , otpAttemptsExceeded :: Boolean
    , modalType :: KeyboardModalType
    , isValidAlternateNumber :: Boolean
    , showResendOtpButton :: Boolean
    , textBoxConfig :: TextBoxConfig
}

type TextBoxConfig = {
  textBoxesArray :: Array Int
  , width :: String
  , height :: String
}

type TextConfig =
  { text :: String
    , focusIndex :: Int
    , gravity :: String
    , visibility :: String
    , color :: String
    , height :: String
    , width :: String
    , cornerRadius :: Number
    , padding :: String
    , margin :: String
    , weight :: Number
    , textStyle :: Style
  }

type ImageConfig =
  {
    imageUrl :: String
  , height :: String
  , width :: String
  , margin :: String
  , visibility :: String
  , alpha :: Number
  }

type ButtonConfig =
  { margin :: String
  , text :: String
  , color :: String
  , width :: String
  , height :: String
  , cornerRadius :: Number
  , stroke :: String
  , background :: String
  , visibility :: String
  , textStyle :: Style
  }

type Keys = {
  keys :: Array String
}

config :: InAppKeyboardModalState
config = {
    errorConfig : {
      text : ""
    , focusIndex : 0
    , gravity : "center"
    , visibility : "visible"
    , color : Color.red
    , height : "wrap_content"
    , width : "wrap_content"
    , cornerRadius : 0.0
    , padding : "0, 0, 0, 0"
    , margin : "0, 0, 0, 0"
    , weight : 0.0
    , textStyle : Body1
    },
    headingConfig : {
      text : ""
    , focusIndex : 0
    , textStyle : Heading3
    , gravity : "center"
    , visibility : "visible"
    , color : Color.black800
    , height : "wrap_content"
    , width : "wrap_content"
    , cornerRadius : 0.0
    , padding : "0, 0, 0, 0"
    , margin : "16, 0, 0, 0"
    , weight : 0.0
    },
    subHeadingConfig : {
      text : ""
    , focusIndex : 0
    , gravity : "center"
    , textStyle : Tags
    , visibility : "visible"
    , color : Color.black900
    , height : "wrap_content"
    , width : "wrap_content"
    , cornerRadius : 0.0
    , padding : "0, 0, 0, 0"
    , margin : "0, 0, 0, 0"
    , weight : 500.0
    },
    inputTextConfig : {
       text : ""
    , focusIndex : 1
    , textStyle : SubHeading1
    , gravity : "center"
    , visibility : "visible"
    , color : Color.black800
    , height : "wrap_content"
    , width : "match_parent"
    , cornerRadius : 0.0
    , padding : "0, 0, 0, 0"
    , margin : "0, 0, 0, 0"
    , weight : 1.0
    },
    buttonConfig : {
      margin : "0, 0, 0, 0"
    , text : ""
    , textStyle : Heading2
    , color : Color.yellow900
    , width : "match_parent"
    , height : "match_parent"
    , cornerRadius : 8.0
    , stroke : "0,#ffffff"
    , background : Color.black
    , visibility : "visible"
    },
    imageConfig : {
      imageUrl : ""
    , height : "124"
    , width : "124"
    , margin : "0, 0, 0, 0"
    , visibility : "visible"
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
      width : "48",
      height : "56"
  }
  }
