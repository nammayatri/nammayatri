{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BannerCarousel.Controller where

import Data.Array
import Data.Maybe
import Data.String
import Prelude

import Font.Style (Style(..))
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM (Length(..), Margin(..), Padding(..), Prop, toPropValue)
import PrestoDOM.List (ListItem)
import Screens.Types (Gender)
import Styles.Colors as Color


data Action = OnClick Int
            | NoAction

data BannerType = AutoPay
  | Disability
  | Gender

type CarouselConfig a = {
    item :: ListItem
 ,  bannerDatas :: Array (Config a)
}

type Config a = {
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
  showActionArrow :: Boolean,
  alertText :: String,
  alertTextColor :: String,
  alertTextStyle :: Style,
  alertTextVisibility :: Boolean,
  padding :: Padding,
  margin :: Margin,
  actionTextVisibility :: Boolean,
  titleTextVisibility :: Boolean,
  imagePadding :: Padding,
  action :: Maybe a,
  "type" :: BannerType
}

config :: forall a. a -> Config a
config action = {
    backgroundColor : Color.darkGreen,
    title : "",
    titleColor : Color.darkGreen,
    actionText : "",
    actionTextColor : Color.darkGreen,
    imageUrl : "ic_logo",
    imageHeight : (V 95),
    imageWidth : (V 118),
    isBanner : true,
    actionTextStyle : ParagraphText,
    titleStyle : Body7,
    showActionArrow : true,
    alertText : "",
    alertTextColor : Color.darkGreen,
    alertTextStyle : Tags,
    alertTextVisibility : false,
    padding : PaddingTop 0,
    margin : MarginTop 12,
    actionTextVisibility : true,
    titleTextVisibility : true,
    imagePadding : PaddingVertical 5 5,
    action: Just action,
    "type" : Gender
}


type PropConfig = (
  backgroundColor :: PropValue,
  alertText :: PropValue,
  alertTextColor :: PropValue,
  visibility :: PropValue,
  titleText :: PropValue,
  alertTextVisibility :: PropValue,
  titleTextColor :: PropValue,
  actionTextVisibility :: PropValue,
  actionText :: PropValue,
  actionTextColor :: PropValue,
  bannerImageUrl :: PropValue,
  cornerRadiusMain :: PropValue
)


bannerTransformer :: forall a. Array (Config a) -> Array (Record PropConfig)
bannerTransformer = map (
  \item -> {
  backgroundColor : toPropValue item.backgroundColor,
  alertText : toPropValue item.alertText,
  alertTextColor : toPropValue item.alertTextColor,
  alertTextVisibility : toPropValue $ if item.alertTextVisibility then "visible" else "gone",
  visibility : toPropValue $ if item.isBanner then "visible" else "gone",
  titleText : toPropValue item.title ,
  titleTextColor : toPropValue item.titleColor,
  actionTextVisibility : toPropValue $ if item.actionTextVisibility then "visible" else "gone",
  actionText : toPropValue item.actionText,
  actionTextColor : toPropValue item.actionTextColor,
  bannerImageUrl : toPropValue $ (fromMaybe "" ((split (Pattern ",") item.imageUrl) !! 0)),
  cornerRadiusMain : toPropValue $ "32.0"
  }
)