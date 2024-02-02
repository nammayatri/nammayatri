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
import Engineering.Helpers.Commons
import Font.Style (Style(..))
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM (Length(..), Margin(..), Padding(..), Prop, toPropValue)
import PrestoDOM.List (ListItem)
import Screens.Types (Gender)
import Styles.Colors as Color
import Common.RemoteConfig (RCCarousel(..))
import Data.String as DS


data Action = OnClick Int
            | NoAction

data BannerType = AutoPay
  | Disability
  | Gender
  | Remote String --TODO:: Temp added for just youtube links
  | ZooTicket
  | Safety
  | MetroTicket

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
  showImageAsCTA :: Boolean,
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
  "type" :: BannerType,
  actionIconUrl :: String,
  actionTextBackgroundColour :: String,
  actionTextCornerRadius :: String,
  actionIconVisibility :: Boolean,
  actionImageUrl :: String,
  actionImageVisibility :: Boolean
}

config :: forall a. a -> Config a
config action = {
    backgroundColor : Color.darkGreen,
    title : "",
    titleColor : Color.darkGreen,
    actionText : "",
    actionTextColor : Color.darkGreen,
    imageUrl : "ic_logo",
    imageHeight : (V 105),
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
    "type" : Gender,
    actionIconUrl : "",
    actionTextBackgroundColour : "",
    actionTextCornerRadius : "",
    actionIconVisibility : false,
    actionImageUrl : "",
    showImageAsCTA : false,
    actionImageVisibility : false
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
  cornerRadiusMain :: PropValue,
  actionIconUrl :: PropValue,
  actionTextBackgroundColour :: PropValue,
  actionTextCornerRadius :: PropValue,
  actionIconVisibility :: PropValue,
  actionImageUrl :: PropValue,
  actionImageVisibility :: PropValue
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
  cornerRadiusMain : toPropValue $ if os == "IOS" then "20.0" else "32.0",
  actionIconUrl : toPropValue item.actionIconUrl,
  actionTextBackgroundColour : toPropValue item.actionTextBackgroundColour,
  actionTextCornerRadius : toPropValue item.actionTextCornerRadius,
  actionIconVisibility : toPropValue $ if item.actionIconVisibility then "visible" else "gone",
  actionImageUrl : toPropValue item.actionImageUrl,
  actionImageVisibility : toPropValue $ if item.actionImageVisibility then "visible" else "gone"
  }
)


remoteConfigTransformer :: forall a. Array RCCarousel -> (Action -> a) -> Array (Config (Action -> a))
remoteConfigTransformer remoteConfig action = 
  map (\(RCCarousel remoteConfig) -> 
    let
      config' = config action
      config'' = config'{
        backgroundColor = remoteConfig.banner_color,
        title = remoteConfig.text,
        titleColor = remoteConfig.text_color,
        actionText = remoteConfig.cta_text,
        actionTextColor = remoteConfig.cta_text_color,
        imageUrl = remoteConfig.banner_image,
        "type" = Remote remoteConfig.cta_link,
        actionIconUrl = remoteConfig.cta_icon,
        actionIconVisibility = not $ DS.null remoteConfig.cta_text,
        actionTextBackgroundColour = remoteConfig.cta_background_color,
        actionTextCornerRadius = remoteConfig.cta_corner_radius,
        actionImageUrl = remoteConfig.cta_image_url,
        showImageAsCTA = not $ DS.null remoteConfig.cta_image_url,
        actionImageVisibility = not $ DS.null remoteConfig.cta_image_url,
        actionTextVisibility = DS.null remoteConfig.cta_image_url 
      }
    in config'') remoteConfig