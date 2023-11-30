{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.StatsModel.View where

import Prelude (Unit, const, map, ($),(-),unit, (*),(/), (+), (<>), (==), show)
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, visibility, stroke, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Components.StatsModel.Controller (Action(..), Config)
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Screens.Types (VehicalTypes(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Properties(cornerRadii)
import Language.Strings (getString)
import Language.Types(STR(..))
import Engineering.Helpers.Commons (screenWidth)
import Common.Types.App
import Helpers.Utils(fetchImage, FetchImageFrom(..))
import ConfigProvider

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
--   , stroke $ "1," <> Color.black500
--   , cornerRadius 10.0
  , visibility config.visibility
  ][earningsView config push]

earningsView :: forall w. Config -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
earningsView config push =
  let feature = (getAppConfig appConfig).feature
  in 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , background Color.blue600
  , gravity CENTER
  , padding $ PaddingVertical 10 10
  , cornerRadius 9.0
  ][  linearLayout
      [ height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , weight 1.0
      ][ textView $ 
         [ height config.countTextConfig.height
         , text config.countTextConfig.text
         , gravity config.countTextConfig.gravity
         , color config.countTextConfig.color
         ] <> FontStyle.tags TypoGraphy
      , textView $
         [ height config.textConfig.height
         , text $ show $ config.totalRidesOfDay
         , gravity config.textConfig.gravity
         , width WRAP_CONTENT
         , color config.textConfig.color
         ] <> FontStyle.h2 TypoGraphy
      ]
    , linearLayout
      [ width (V 1)
      , height (V 42)
      , background Color.grey900
      ][]
    , linearLayout
      [ height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , weight 1.0
      ][ textView $ 
         [ height config.earningsTextConfig.height
         , text config.earningsTextConfig.text
         , gravity config.earningsTextConfig.gravity
         , color config.earningsTextConfig.color
         , textSize FontSize.a_12
         ]<> FontStyle.tags TypoGraphy
      , textView $ 
         [ height config.textConfig.height
         , text $ (getCurrency appConfig) <> (show $ config.totalEarningsOfDay)
         , gravity config.textConfig.gravity
         , color config.textConfig.color
         ]<> FontStyle.h2 TypoGraphy
      ]
    , linearLayout
      [ width (V 1)
      , height (V 42)
      , background Color.grey900
      , visibility if feature.enableBonus then VISIBLE else GONE
      ][]
    , linearLayout
      [ height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , weight 1.0
      , visibility if feature.enableBonus then VISIBLE else GONE
      ][ textView $ 
         [ height config.bonusTextConfig.height
         , text config.bonusTextConfig.text
         , gravity config.bonusTextConfig.gravity
         , color config.earningsTextConfig.color
         , textSize FontSize.a_12
         ]<> FontStyle.tags TypoGraphy
      ,  linearLayout
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , gravity CENTER
         ]
         [ textView $ 
            [ height config.textConfig.height
            , text $ (getCurrency appConfig) <> (show $ config.bonusEarned)
            , color config.bonusTextConfig.color
            ]<> FontStyle.h2 TypoGraphy
         , imageView
            [imageWithFallback $ fetchImage FF_ASSET "ic_info"
            , width $ V 14
            , height $ V 23
            , margin $ Margin 5 2 0 0
            , onClick push $ const OnIconClick
            ]
         ]
      ]
   ]