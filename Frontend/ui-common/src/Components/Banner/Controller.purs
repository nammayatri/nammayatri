{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.Banner.Controller where

import Styles.Colors as Color
import PrestoDOM (Length(..), Margin(..), Padding(..))
import Font.Style(Style(..))


data Action = OnClick
            | NoAction


type Config = {
  backgroundColor :: String,
  title :: String,
  titleColor :: String,
  actionText :: String,
  actionTextColor :: String,
  imageUrl :: String,
  imageHeight :: Length,
  imageWidth :: Length,
  isBanner :: Boolean,
  actionTextStyle :: Style,
  titleStyle :: Style,
  stroke :: String,
  showActionArrow :: Boolean,
  alertText :: String,
  alertTextColor :: String,
  alertTextStyle :: Style,
  alertTextVisibility :: Boolean,
  bannerClickable :: Boolean,
  padding :: Padding,
  margin :: Margin,
  actionTextVisibility :: Boolean,
  actionTextBackgroundColor :: String,
  actionTextCornerRadius :: Number,
  titleTextVisibility :: Boolean,
  imagePadding :: Padding
}

config :: Config
config = {
    backgroundColor : Color.darkGreen,
    title : "",
    titleColor : Color.darkGreen,
    actionText : "",
    actionTextColor : Color.darkGreen,
    imageUrl : "",
    imageHeight : (V 95),
    imageWidth : (V 118),
    isBanner : true,
    actionTextStyle : ParagraphText,
    titleStyle : Body7,
    stroke : "0,#FFFFFF",
    showActionArrow : true,
    alertText : "",
    alertTextColor : "",
    alertTextStyle : Tags,
    alertTextVisibility : false,
    bannerClickable : true,
    padding : PaddingTop 0,
    margin : MarginTop 12,
    actionTextVisibility : true,
    actionTextBackgroundColor : "00FFFFFF",
    actionTextCornerRadius : 0.0,
    titleTextVisibility : true,
    imagePadding : PaddingVertical 5 5
}
