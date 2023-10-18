{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.MenuButton.Controller where

import Prelude((<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Font.Size as FontSize
import Font.Style (Style(..))
import Common.Styles.Colors as Color
import Common.Types.App

data Action = OnClick Config

type Config =
  {
      titleConfig :: TextConfig
    , subTitleConfig :: TextConfig
    , id :: String
    , index :: Int
    , width :: Length
    , height :: Length
    , cornerRadius :: Number
    , padding :: Padding
    , stroke :: String
    , isClickable :: Boolean
    , visibility :: Visibility
    , background :: String
    , isSelected :: Boolean
    , radioButtonConfig :: RadioButtonConfig
    , leftsidebutton :: Boolean
    , lat :: Number
    , lng :: Number
    , layoutBg :: String 
    , layoutStroke :: String 
    , accessibilityHint :: String
  }

type TextConfig =
  { text :: String
  , selectedTextStyle :: Style
  , unselectedTextStyle :: Style
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , singleLine :: Boolean
  }


type RadioButtonConfig =
  { height :: Length
  , width :: Length
  , buttonHeight :: Length
  , buttonWidth :: Length
  , buttonMargin :: Margin
  , buttonPadding :: Padding
  , activeStroke :: String
  , inActiveStroke :: String
  , cornerRadius :: Number
  , margin :: Margin
  , padding :: Padding
  , buttonColor :: String
  , buttonCornerRadius :: Number
  }

config :: Config
config =
  {
      titleConfig :
          { text : ""
          , selectedTextStyle : SubHeading2
          , unselectedTextStyle : Body1
          , gravity : LEFT
          , visibility : VISIBLE
          , color : Color.black800
          , singleLine : true
          }
    , subTitleConfig :
          { text : ""
          , selectedTextStyle : Body1
          , unselectedTextStyle : Body1
          , gravity : LEFT
          , visibility : VISIBLE
          , color : Color.black700
          , singleLine : true
          }
    , width : MATCH_PARENT
    , height : V 70
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , stroke : ""
    , isClickable : true
    , visibility : VISIBLE
    , background : ""
    , isSelected : false
    , radioButtonConfig :
          { height : V 20
          , width : V 20
          , buttonHeight : V 10
          , buttonWidth : V 10
          , buttonMargin : (Margin 0 0 0 0)
          , buttonPadding : (Padding 0 0 0 0)
          , activeStroke :("2," <> Color.black800)
          , inActiveStroke :("2," <> Color.black600)
          , cornerRadius : 10.0
          , margin : (Margin 0 10 0 10)
          , padding : (Padding 0 0 0 0)
          , buttonColor : Color.black800
          , buttonCornerRadius : 5.0
          }
    , index : 0
    , id : ""
    , leftsidebutton : false
    , lat : 0.0
    , lng : 0.0
    , layoutBg : "" 
    , layoutStroke : "0," <> Color.grey900
    , accessibilityHint : ""
  }

