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
import Styles.Colors as Color
import Font.Style (Style(..))
import Data.Maybe(Maybe(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), LetterSpacing(..))
import Common.Types.App

data Action = TextChanged String String

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
  , textStyle :: Style
  }

type TextConfig =
  { text :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , alpha :: Number
  , margin :: Margin
  , textStyle :: Style
  }
type ImageConfig =
  { height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding
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
    , placeholderColor : Color.blueGrey
    , singleLine : true
    , pattern : Nothing
    , margin : (Margin 20 0 10 0)
    , padding : (Padding 0 16 16 16)
    , letterSpacing : PX 0.0
    , alpha : 1.0
    , capsLock : false
    }
  , visibility : VISIBLE
  , background : Color.white900
  , cornerRadius : 5.0
  , stroke : ("1," <> Color.borderColorLight)
  , warningStroke : ("1," <> Color.lightMaroon)
  , id : ""
  , topLabel :
    { text : ""
    , textStyle : Body3
    , gravity : LEFT
    , visibility : VISIBLE
    , color : Color.yellow900
    , alpha : 1.0
    , margin : (Margin 0 0 0 10)
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
    }
  , showErrorImage : false
  , errorImageConfig :
  {   height : V 10
    , width : V 10
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
  }

}
