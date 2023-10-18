{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SelectMenuButton.View where

import Common.Types.App

import Common.Types.App (LazyCheck(..))
import Components.SelectMenuButton.Controller (Action(..), State)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>), (||), not)
import Prelude (Unit, const, ($), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width)
import Styles.Colors as Color

view 
  :: forall w.(Action -> Effect Unit)
  -> State
  -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.greySmoke
      , margin (Margin 20 5 20 5)
      , visibility if state.index == 0 || (not state.lineVisiblity) then GONE else VISIBLE
      ][]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , padding (Padding 16 24 0 0)
          , onClick push (const (OnSelection state))
          , gravity CENTER_VERTICAL
          ][ linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation VERTICAL
              ][textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text state.text.name
                , color Color.greyTextColor
                ] <> if state.isSelected then FontStyle.subHeading2 TypoGraphy else FontStyle.body5 TypoGraphy
                , textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text state.text.subtitle
                ] <> if state.isSelected then FontStyle.subHeading2 TypoGraphy else FontStyle.body5 TypoGraphy
              ]
            ,linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , weight 1.0
              , gravity RIGHT
              , margin (MarginRight 20)
              ][frameLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  ][ imageView
                      [ height (V 24)
                      , width (V 24)
                      , imageWithFallback $ fetchImage FF_COMMON_ASSET $ "ny_ic_radio_selected"
                      , visibility if state.isSelected then VISIBLE else GONE
                      ]
                    , imageView
                      [ width (V 24)
                      , height (V 24)
                      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_radio_unselected"
                      , visibility if state.isSelected then GONE else VISIBLE
                      ]
                  ]
              ]
          ]
    ]