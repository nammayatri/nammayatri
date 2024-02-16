{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RequestInfoCard.Controller where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App

data Action = Close 
            | BackPressed 
            | NoAction

type Config = {
    title :: TextConfig,
    primaryText :: TextConfig,
    secondaryText :: TextConfig,
    imageConfig :: ImageConfig,
    buttonConfig :: ButtonConfig
}

type ImageConfig = {
    visibility :: Visibility,
    imageUrl :: String,
    height :: Length,
    width :: Length,
    margin :: Margin,
    padding :: Padding
}

type TextConfig = {
  text :: String,
  color :: String,
  padding :: Padding,
  margin :: Margin,
  visibility :: Visibility,
  textStyle :: FontStyle.Style,
  height :: Length,
  width :: Length,
  accessibilityHint :: String
}

type ButtonConfig = {
  text :: String,
  color :: String,
  visibility :: Visibility,
  margin :: Margin,
  padding :: Padding,
  gravity :: Gravity,
  textStyle :: FontStyle.Style,
  accessibilityHint :: String
}

config :: Config
config = {
    title : {
      text : "",
      color : Color.black800,
      padding : Padding 16 24 0 0,
      margin : Margin 0 0 0 0,
      visibility : VISIBLE,
      textStyle : FontStyle.SubHeading1,
      height : WRAP_CONTENT,
      width : WRAP_CONTENT,
      accessibilityHint : ""
    }
  , primaryText : {
      text : "",
      color : Color.black700,
      textStyle : FontStyle.ParagraphText,
      padding : Padding 16 16 0 0,
      margin : Margin 0 0 0 0,
      visibility : VISIBLE,
      height : WRAP_CONTENT,
      width : WRAP_CONTENT,
      accessibilityHint : ""
    }
  , secondaryText : {
      text : "",
      color : Color.black700,
      padding : Padding 16 16 0 0,
      margin : Margin 0 0 0 0,
      visibility : GONE,
      textStyle : FontStyle.SubHeading1,
      height : WRAP_CONTENT,
      width : WRAP_CONTENT,
      accessibilityHint : ""
    }
  , imageConfig : {
      visibility : VISIBLE,
      imageUrl : "",
      height : V 122,
      width : V 116,
      margin : Margin 0 0 0 0,
      padding : Padding 0 0 0 0
    }
  , buttonConfig : {
      text : "",
      color : Color.blue800,
      visibility : VISIBLE,
      margin : Margin 0 0 0 0,
      padding : PaddingVertical 28 20,
      textStyle : FontStyle.SubHeading1,
      gravity : CENTER,
      accessibilityHint : ""
    }
}