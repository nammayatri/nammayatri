{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.IncrementDecrement.View (app)  where

import Prelude
import React.Basic.Hooks (Component, component)
import React.Basic.Roll (jempty, (+=))
import Effect (Effect)
import React.Render.CustomBase (linearLayout, textView)
import Styles.Colors as Color
import ReactComponents.IncrementDecrement.Controller (Config)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useEffect, useState)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console (log)
import Halogen.VDom.Types (VDom(..))
import Data.Maybe (fromJust, fromMaybe, Maybe(..), maybe)
import PrestoDOM.Properties (background, color, cornerRadius, fontStyle, gravity, height, imageWithFallback, layoutGravity, margin, orientation, padding, text, textSize, visibility, weight, width)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))

app :: forall action . (Int -> action) -> (action -> Effect Unit) -> Component Config
app action push = do 
  component "app" \{initialCount, plus, minus, stroke, backgroundColor, fontSize, cornerRadius, padding, margin, onChange} -> React.do
    count /\ setCount <- useState initialCount
    let onIncrement = setCount (\prevCount -> prevCount + 1)
    let onDecrement = setCount (\prevCount -> if(prevCount > 1) then prevCount - 1 else prevCount)

    useEffect count (\_ -> do
                      log ("IncrementDecrementView count: " <> show count)
                      _ <- maybe (push $ action count) (\f -> f count) onChange
                      pure $ pure unit)

    pure $
      linearLayout [
          height MATCH_PARENT
        , width MATCH_PARENT
        -- , orientation "vertical"
        -- , weight "0.0"
        -- , margin margin
      ] (jempty)
      -- linearLayout {
      --       height: "wrap_content"
      --     , width: "match_parent"
      --     , orientation: "horizontal"
      --     , padding: "4, 4, 4, 4"
      --     , cornerRadius: "8.0"
      --     , background: backgroundColor
      --     , stroke: stroke
      --   } [ textView {
      --         background: minus.backgroundColor
      --       , text: "-"
      --       , textSize: fontSize
      --       , gravity: "center"
      --       , cornerRadius: cornerRadius
      --       , width: minus.width
      --       , padding: padding
      --       , onClick: onDecrement
      --       , height: minus.height
      --       , rippleColor: minus.rippleColor
      --       }
      --     , textView {
      --         background: Color.white900
      --       , text: (show count)
      --       , textSize: fontSize
      --       , height: "wrap_content"
      --       , color: Color.black800
      --       , padding: padding
      --       , weight: "1.0"
      --       , gravity: "center"
      --       } 
      --     , textView {
      --         background: plus.backgroundColor
      --       , width: plus.width
      --       , height: plus.height
      --       , text: "+"
      --       , textSize: fontSize
      --       , color: plus.textColor
      --       , padding: padding
      --       , cornerRadius: cornerRadius
      --       , onClick: onIncrement
      --       , rippleColor: plus.rippleColor
      --       }
      --     ]
      --   ]