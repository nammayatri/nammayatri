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
import Common.Styles.Colors as Color
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
  fontSize :: Int,
  padding :: Padding,
  margin :: Margin,
  visibility :: Visibility,
  fontStyle :: String
}

type ButtonConfig = {
  text :: String,
  color :: String,
  fontSize :: Int,
  visibility :: Visibility,
  margin :: Margin,
  padding :: Padding,
  fontStyle :: String,
  gravity :: Gravity
}

config :: Config
config = {
    title : {
      text : "",
      color : Color.black800,
      fontSize : FontSize.a_16,
      padding : Padding 16 24 0 0,
      margin : Margin 0 0 0 0,
      visibility : VISIBLE,
      fontStyle : FontStyle.semiBold LanguageStyle
    }
  , primaryText : {
      text : "",
      color : Color.black700,
      fontSize : FontSize.a_14,
      padding : Padding 16 16 0 0,
      margin : Margin 0 0 0 0,
      visibility : VISIBLE,
      fontStyle : FontStyle.regular LanguageStyle
    }
  , secondaryText : {
      text : "",
      color : Color.black700,
      fontSize : FontSize.a_16,
      padding : Padding 16 16 0 0,
      margin : Margin 0 0 0 0,
      visibility : GONE,
      fontStyle : FontStyle.semiBold LanguageStyle
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
      fontSize : FontSize.a_16,
      visibility : VISIBLE,
      margin : Margin 0 0 0 0,
      padding : PaddingVertical 28 20,
      fontStyle : FontStyle.semiBold LanguageStyle,
      gravity : CENTER
    }
}