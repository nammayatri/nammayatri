{-
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IncrementDecrementModel.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.IncrementDecrementModel.Controller (Action(..), Config)
import Effect (Effect)
import Font.Style as FontStyle
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, background, color, cornerRadius, gravity, height, linearLayout, margin, onClick, orientation, padding, rippleColor, stroke, text, textView, visibility, weight, width)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height WRAP_CONTENT
  , width config.width
  , orientation VERTICAL
  , gravity config.gravity
  , background config.background
  , cornerRadius config.cornerRadius
  , padding config.padding
  ]
  [ textView $
    [ text config.heading
    , color Color.black800
    , margin $ MarginTop 16
    , visibility $ boolToVisibility $ config.heading /= ""
    ] <> FontStyle.body1 TypoGraphy
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation config.orientation
    , gravity config.gravity
    ]
    [ textView $
      [ background config.buttonTextConfig.background
      , text $ "-"
      , color config.buttonTextConfig.color
      , cornerRadius config.buttonTextConfig.cornerRadius
      , padding config.buttonTextConfig.padding
      , background config.buttonTextConfig.background
      , gravity config.buttonTextConfig.gravity
      , onClick push $ const OnDecrement
      , margin config.buttonTextConfig.margin
      ] <> config.buttonTextConfig.fontStyle
    , textView $
        [ background config.countTextConfig.background
        , text $ show config.initialValue <> " km"
        , color config.countTextConfig.color
        , padding config.countTextConfig.padding
        , margin config.countTextConfig.margin
        , cornerRadius config.countTextConfig.cornerRadius
        , gravity $ CENTER
        , weight 1.0
        ] <> config.countTextConfig.fontStyle
    , textView $
      [ background config.buttonTextConfig.background
      , text $ "+"
      , color config.buttonTextConfig.color
      , cornerRadius config.buttonTextConfig.cornerRadius
      , padding config.buttonTextConfig.padding
      , cornerRadius config.buttonTextConfig.cornerRadius
      , gravity config.buttonTextConfig.gravity
      , margin config.buttonTextConfig.margin
      , onClick push $ const OnIncrement
      ] <> config.buttonTextConfig.fontStyle
    ]
  ]