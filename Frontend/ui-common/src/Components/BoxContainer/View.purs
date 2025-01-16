{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.BoxContainer.View where

import Components.BoxContainer.Controller
import Data.Maybe
import Prelude
import PrestoDOM.List
import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onClick, orientation, padding, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, root, backgroundColor, alpha)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons
import Components.InfoBox as InfoBox
import Data.String (null)
import Mobility.Prelude (boolToVisibility)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 16
    , margin config.margin
    , cornerRadius 16.0
    , onClick push $ const $ OnClick config.buttonAction
    , background Color.blue600
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ textContainer push config
        , toggleView push config
        ]
    , InfoBox.view push $ infoBoxConfig config
    ]

textContainer :: forall w a. (a -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
textContainer push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ textView -- Title
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text config.title
          , color Color.black800
          ]
        <> FontStyle.body5 TypoGraphy
    , textView -- SubTitle
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , visibility $ boolToVisibility $ not null config.subTitle
          , text config.subTitle
          , color Color.black700
          ]
        <> FontStyle.body5 TypoGraphy
    ]

toggleView :: forall w a. (a -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
toggleView push config =
  let
    backgroundColor = if config.toggleButton then Color.green900 else Color.black600

    align = if config.toggleButton then RIGHT else LEFT
  in
    linearLayout
      [ width $ V 45
      , height $ V 27
      , cornerRadius 100.0
      , background backgroundColor
      , stroke $ "1," <> backgroundColor
      , gravity CENTER_VERTICAL
      , clickable $ not true
      ]
      [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity align
          ]
          [ linearLayout
              [ width $ V 20
              , height $ V 20
              , background Color.white900
              , cornerRadius 100.0
              , gravity CENTER_VERTICAL
              , margin (MarginHorizontal 2 2)
              ]
              []
          ]
      ]
  where
  getAction :: Action
  getAction = NoAction
