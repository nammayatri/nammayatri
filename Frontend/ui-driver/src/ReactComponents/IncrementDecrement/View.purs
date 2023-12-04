{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.IncrementDecrement.View (app) where

import Prelude (Unit, show, pure, (<>), ($))
import React.Basic.Hooks (Component, component)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console (log)
import Effect (Effect)
import React.Render.CustomBase
import Styles.Colors as Color
import ReactComponents.IncrementDecrement.Controller (IncrementDecrementState)

app :: Component IncrementDecrementState
app = do 
  component "app" \{count, onIncrement, onDecrement} ->
    pure $
      linearLayout {
          height: "wrap_content"
        , width: "match_parent"
        , orientation: "vertical"
        , weight: "0.0"
        , margin: "0, 24, 0, 24"
      } [ linearLayout {
            height: "wrap_content"
          , width: "match_parent"
          , orientation: "horizontal"
          , padding: "4, 4, 4, 4"
          , cornerRadius: "8.0"
          , background: Color.white900
          , stroke: "1," <> Color.grey700
        } [ textView {
              background: Color.grey700
            , text: "-"
            , textSize: "16"
            , gravity: "center"
            , cornerRadius: "4.0"
            , width: "wrap_content"
            , padding: "28, 7, 28, 7"
            , onClick: onDecrement
            , height: "wrap_content"
            , rippleColor: Color.black900
            }
          , textView {
              background: Color.white900
            , text: (show count)
            , textSize: "16"
            , height: "wrap_content"
            , color: Color.black800
            , padding: "28, 7, 28, 7"
            , weight: "1.0"
            , gravity: "center"
            } 
          , textView {
              background: Color.black900
            , width: "wrap_content"
            , height: "wrap_content"
            , gravity: "bottom"
            , text: "+"
            , textSize: "16"
            , color: Color.yellow900
            , padding: "28, 7, 28, 7"
            , cornerRadius: "4.0"
            , onClick: onIncrement
            , rippleColor: Color.grey700
            }
          ]
        ]