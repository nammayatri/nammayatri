{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.Banner.View where

import Prelude
import Effect (Effect)
import PrestoDOM ( Margin(..), Orientation(..), Padding(..), Visibility(..), Length(..), PrestoDOM, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, text, textSize, textView, weight, width, padding, visibility, afterRender, editText, onClick, alignParentBottom, imageWithFallback )
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Font.Style as FontStyle
import Font.Size as FontSize
import Components.Banner.Controller
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Commons (os, screenWidth)


view :: forall w. (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 12.0
    , background config.backgroundColor
    , visibility if config.isBanner then VISIBLE else GONE
    , onClick push (const OnClick)
    ]
    [  linearLayout
        [ height MATCH_PARENT
        , weight 1.0
        , padding $ Padding 20 (if os == "IOS" then 16 else 13) 0 18 
        , orientation VERTICAL
        , gravity CENTER_VERTICAL
        ]
        [ textView
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity LEFT
          , text config.title
          , color config.titleColor
          , fontStyle $ FontStyle.bold LanguageStyle
          , textSize if(screenWidth unit < 380) then  FontSize.a_14 else  FontSize.a_15
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , margin $ MarginTop if (os == "IOS") then 4 else 0
          ]
          [
            textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity LEFT
            , text $ config.actionText <> " →" 
            , color config.actionTextColor
            , textSize if(screenWidth unit < 380) then  FontSize.a_12 else  FontSize.a_13
            , fontStyle $ FontStyle.regular LanguageStyle
            ]
          ]
        ]
    ,   linearLayout
     [
        height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
     ][imageView
        [
          height $ V 95
        , width $ V 108
        , margin $ MarginRight 5
        , imageWithFallback config.imageUrl
        ]]
    ]