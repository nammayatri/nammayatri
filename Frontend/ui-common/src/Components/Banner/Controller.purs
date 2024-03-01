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
import Data.Maybe (Maybe(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Prelude (class Eq)


data Action = OnClick
            | NoAction


type Config = {
  backgroundColor :: String,
  title :: String,
  titleColor :: String,
  imageUrl :: String,
  imageHeight :: Length,
  imageWidth :: Length,
  isBanner :: Boolean,
  titleStyle :: Style,
  stroke :: String,
  showActionArrow :: Boolean,
  alertTextVisibility :: Boolean,
  bannerClickable :: Boolean,
  padding :: Padding,
  margin :: Margin,
  actionTextVisibility :: Boolean,
  titleTextVisibility :: Boolean,
  imagePadding :: Padding,
  cornerRadius :: Number,
  imageMargin :: Margin,
  actionText :: TextConfig,
  alertText :: TextConfig,
  actionTextImgType :: ArrowType 
}

data ArrowType = DownArrow | RightArrow

derive instance genericArrowType :: Generic ArrowType _ 
instance eqArrowType :: Eq ArrowType where eq = genericEq

type TextConfig = {
  backgroundColor :: Maybe String,
  cornerRadius :: Number,
  padding :: Padding,
  margin :: Margin,
  style :: Style,
  text :: String,
  textColor :: String 
}

config :: Config
config = {
    backgroundColor : Color.darkGreen,
    title : "",
    titleColor : Color.darkGreen,
    imageUrl : "",
    imageHeight : (V 95),
    imageWidth : (V 118),
    isBanner : true,
    titleStyle : Body7,
    stroke : "0,#FFFFFF",
    showActionArrow : true,
    alertTextVisibility : false,
    bannerClickable : true,
    padding : PaddingTop 0,
    margin : MarginTop 12,
    actionTextVisibility : true,
    titleTextVisibility : true,
    imagePadding : PaddingVertical 5 5,
    imageMargin : MarginRight 5,
    cornerRadius : 12.0,
    actionTextImgType : RightArrow,
    actionText : {
      backgroundColor : Just "00FFFFFF" ,
      cornerRadius : 0.0,
      padding : PaddingHorizontal 0 0,
      margin : MarginTop 0,
      style : ParagraphText,
      text : "",
      textColor : Color.darkGreen
    },
    alertText : {
      backgroundColor : Nothing ,
      cornerRadius : 0.0,
      padding : PaddingHorizontal 0 0,
      margin : MarginTop 0,
      style : Tags,
      text : "",
      textColor : ""
    }
}
