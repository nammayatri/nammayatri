{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PSBanner.View where

import Components.PSBanner.Controller
import Prelude
import PrestoDOM.List

import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Styles.Colors as Color
import Data.Maybe

view :: forall w a. (a -> Effect Unit) -> (Config (Action -> a)) -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginHorizontal 16 16
    ][ bannerView push config]



bannerView :: forall w a. (a -> Effect Unit) -> (Config (Action -> a)) -> PrestoDOM (Effect Unit) w
bannerView push config = 
  linearLayout
    ([ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadiusHolder "cornerRadiusMain"
    , backgroundHolder "backgroundColor"
    , visibilityHolder "visibility" --if config.isBanner then VISIBLE else GONE
    , gravity CENTER
    ] <> maybe ([]) (\action -> [onClickHolder push $ (action <<< OnClick)]) config.action)
    [ linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , padding $ Padding 20 0 0 0
        , orientation VERTICAL
        , layoutGravity "center_vertical"
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity LEFT
              , textHolder "alertText"
              , colorHolder "alertTextColor"
              , padding $ PaddingBottom 2
              , visibilityHolder "alertTextVisibility"
              ]
            <> (FontStyle.getFontStyle config.alertTextStyle LanguageStyle)
        , textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity LEFT
              , textHolder "titleText"
              , colorHolder "titleTextColor"
              , padding $ PaddingBottom 2
              , visibility if config.titleTextVisibility then VISIBLE else GONE
              ]
            <> (FontStyle.getFontStyle config.titleStyle LanguageStyle)
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , visibilityHolder "actionTextVisibility" --if config.actionTextVisibility then VISIBLE else GONE
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , gravity LEFT
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
                  , color config.actionTextColor
                  , padding $ PaddingBottom 3
                  , margin $ MarginLeft 5
                  -- , visibility if config.showActionArrow then VISIBLE else GONE
                  ]
                <> (FontStyle.getFontStyle config.actionTextStyle LanguageStyle)
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , padding $ PaddingVertical 5 5
        ]
        [ imageView
            [ height config.imageHeight
            , width config.imageWidth
            , margin $ MarginRight 5
            , imageUrlHolder "bannerImageUrl"
            ]
        ]
    ]
