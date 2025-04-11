{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PrimaryEditText.Controller where

import Prelude((<>))
import Font.Size as FontSize
import Common.Styles.Colors as Color
import Font.Style (Style(..)) 
import Data.Maybe(Maybe(..))
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), LetterSpacing(..), Accessiblity(..))
import Common.Types.App

data Action = TextChanged String String | FocusChanged Boolean | TextImageClicked

type Config =
  { height :: Length
  , width :: Length
  , editText :: EditTextConfig
  , visibility :: Visibility
  , background :: String
  , cornerRadius :: Number
  , stroke :: String
  , id ::String
  , topLabel :: TextConfig
  , showErrorLabel :: Boolean
  , errorLabel :: TextConfig
  , showErrorImage :: Boolean
  , errorImageConfig :: ImageConfig
  , margin :: Margin
  , type :: String
  , warningStroke :: String
  , constantField :: ConstantFieldConfig
  , showConstantField :: Boolean
  , focusedStroke :: String
  , textImage :: ImageConfig
  }

type ConstantFieldConfig = {
    width :: Length
  , height :: Length
  , gravity :: Gravity
  , text :: String
  , textStyle :: Style
  , color :: String
  , padding :: Padding
  , margin :: Margin
}

type EditTextConfig =
  { text :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , placeholder :: String
  , placeholderColor :: String
  , singleLine :: Boolean
  , pattern :: Maybe String
  , margin :: Margin
  , padding :: Padding
  , letterSpacing :: LetterSpacing
  , alpha :: Number
  , capsLock :: Boolean
  , enabled :: Boolean
  , focused :: Boolean
  , textStyle :: Style
  , separatorRepeat :: String
  , separator :: String
  , accessibilityHint :: String
  }

type TextConfig =
  { text :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , alpha :: Number
  , margin :: Margin
  , textStyle :: Style
  , accessibility :: Accessiblity
  }
type ImageConfig =
  { height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding
  , visibility :: Visibility
  }

config :: Config
config = {
    height : V 54
  , width : MATCH_PARENT
  , margin : (Margin 10 0 10 15)
  , type : ""
  , editText :
    { text : ""
    , textStyle : SubHeading1
    , color : Color.black800
    , gravity : LEFT
    , visibility : VISIBLE
    , placeholder : ""
    , placeholderColor : Color.darkGrey
    , singleLine : true
    , pattern : Nothing
    , margin : (Margin 20 0 10 0)
    , padding : (Padding 0 16 16 16)
    , letterSpacing : PX 0.0
    , alpha : 1.0
    , capsLock : false
    , enabled : true
    , focused : false
    , separatorRepeat : ""
    , separator : ""
    , accessibilityHint : ""
    }
  , visibility : VISIBLE
  , background : Color.white900
  , cornerRadius : 5.0
  , stroke : ("1," <> Color.borderColorLight)
  , warningStroke : ("1," <> Color.lightMaroon)
  , focusedStroke : ("1," <> Color.blue800)
  , id : ""
  , topLabel :
    { text : ""
    , textStyle : Body3
    , gravity : LEFT
    , visibility : VISIBLE
    , color : Color.yellow900
    , alpha : 1.0
    , margin : (Margin 0 0 0 10)
    , accessibility : ENABLE
    }
  , showErrorLabel : false
  , errorLabel : {
      text : ""
    , gravity : LEFT
    , textStyle : Body3
    , color : Color.lightMaroon
    , visibility : GONE
    , alpha : 1.0
    , margin : (Margin 0 0 0 0)
    , accessibility : ENABLE
    }
  , showErrorImage : false
  , errorImageConfig :
  {   height : V 10
    , width : V 10
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
    , visibility : VISIBLE
  }
  , showConstantField : false
  , constantField : {
    width : WRAP_CONTENT
  , height : MATCH_PARENT
  , gravity : CENTER
  , text : "+91"
  , textStyle : ParagraphText
  , color : Color.black800
  , padding : Padding 0 0 0 0
  , margin : Margin 10 0 0 0
  }
  , textImage : {
      height : V 24
    , width : V 24
    , imageUrl : fetchImage FF_COMMON_ASSET "ny_ic_id_filled"
    , margin : (Margin 16 0 16 0)
    , padding : (Padding 0 0 0 0)
    , visibility : GONE
  }
}
