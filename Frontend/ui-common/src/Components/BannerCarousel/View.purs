{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.BannerCarousel.View where

import Components.BannerCarousel.Controller
import Data.Maybe
import Prelude
import PrestoDOM.List
import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onClick, orientation, padding, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, root, backgroundColor)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Styles.Colors as Color


view :: forall w a. (a -> Effect Unit) -> (Config (Action -> a)) -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ PaddingHorizontal 16 16
    , root true
    ][ bannerView push config]



bannerView :: forall w a. (a -> Effect Unit) -> (Config (Action -> a)) -> PrestoDOM (Effect Unit) w
bannerView push config = 
  linearLayout
    ([ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadiusHolder "cornerRadiusMain"
    , backgroundHolder "backgroundColor"
    , visibilityHolder "visibility"
    , gravity CENTER
    ] <> maybe ([]) (\action -> [onClickHolder push $ (action <<< OnClick)]) config.action)
    [ linearLayout
        [ height WRAP_CONTENT
        , padding $ Padding 20 0 0 2
        , orientation VERTICAL
        , layoutGravity "center_vertical"
        , weight 1.0
        ]
        [ 
          textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity LEFT
              , textHolder "alertText"
              , colorHolder "alertTextColor"
              , visibilityHolder "alertTextVisibility"
              , singleLine false
              ]
            <> (FontStyle.getFontStyle config.alertTextStyle LanguageStyle)
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity LEFT
              , textHolder "titleText"
              , colorHolder "titleTextColor"
              , singleLine false
              , visibility if config.titleTextVisibility then VISIBLE else GONE
              ]
            <> (FontStyle.getFontStyle config.titleStyle LanguageStyle)
        , imageView
          [ height $ V 40
          , width $ V 120
          , margin $ MarginTop 5
          , imageUrlHolder "actionImageUrl"
          , visibilityHolder "actionImageVisibility"
          ]  
          , linearLayout
            [ height $ V 25
            , width WRAP_CONTENT
            , visibility GONE
            , visibilityHolder "actionTextVisibility"
            , padding $ Padding 10 0 10 0
            , margin $ MarginTop 5
            , gravity CENTER_VERTICAL
            , backgroundHolder "actionTextBackgroundColour"
            , cornerRadiusHolder "actionTextCornerRadius"
            ]
            [ imageView
                [ height $ V 10
                , width $ V 15
                , margin $ MarginRight 5
                , imageUrlHolder "actionIconUrl"
                , visibilityHolder "actionIconVisibility"
                ]
            , textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , gravity LEFT
                  , textSize FontSize.a_8
                  , textHolder "actionText"
                  , colorHolder "actionTextColor"
                  , padding $ PaddingBottom 2
                  ]
                <> (FontStyle.getFontStyle config.actionTextStyle LanguageStyle)
            , textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , gravity LEFT
                  , textFromHtml "&rarr;"
                  , colorHolder "actionTextColor"
                  , padding $ PaddingBottom 3
                  , margin $ MarginLeft 5
                  ]
                <> (FontStyle.getFontStyle config.actionTextStyle LanguageStyle)
            ]
            ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , padding $ Padding 0 5 5 5
        ]
        [ imageView
            [ height config.imageHeight
            , width config.imageWidth
            , imageUrlHolder "bannerImageUrl"
            ]
        ]
    ]
