{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ErrorModal.Controller where

import PrestoDOM ( Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.PrimaryButton.Controller as PrimaryButtonController
import Font.Size as FontSize
import Font.Style (Style(..))
import Common.Styles.Colors as Color
import Common.Types.App

data Action = PrimaryButtonActionController PrimaryButtonController.Action

type Config =
  {
    height :: Length
  , gravity :: Gravity
  , background :: String
  , stroke :: String
  , corners :: Corners
  , imageConfig :: ImageConfig
  , errorConfig :: TextConfig
  , errorDescriptionConfig :: TextConfig
  , buttonConfig :: ButtonConfig
  }

type ImageConfig =
  { imageUrl :: String
  , height :: Length
  , width :: Length
  , margin :: Margin
  , visibility :: Visibility
  }

type TextConfig =
  { text :: String
  , textStyle :: Style
  , color :: String 
  , padding :: Padding
  , margin :: Margin
  , visibility :: Visibility
  }

type ButtonConfig =
  { margin :: Margin
  , text :: String
  , textStyle :: Style
  , color :: String 
  , width :: Length
  , height :: Length
  , cornerRadius :: Number
  , stroke :: String
  , background :: String
  , visibility :: Visibility
  , enableRipple :: Boolean
  }

config :: Config
config =
  { height : MATCH_PARENT
  , gravity : CENTER
  , background : Color.white900
  , corners : (Corners 0.0 false false false false)
  , stroke : "0,#000000"
  , imageConfig :
    { imageUrl : ""
    , height : V 124
    , width : V 124
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , errorConfig :
    { text : ""
    , textStyle : Heading2
    , color : Color.black
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , errorDescriptionConfig :
    { text : ""
    , textStyle : ParagraphText
    , color : Color.black
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , buttonConfig :
    { margin : (Margin 0 0 0 0)
    , text : ""
    , textStyle : SubHeading1
    , color : Color.yellow900
    , width : MATCH_PARENT
    , height : V 50
    , cornerRadius : 8.0
    , stroke : "0,#ffffff"
    , background : Color.black
    , visibility : VISIBLE
    , enableRipple : false
    }

  }
