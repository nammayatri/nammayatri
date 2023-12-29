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
import React.Basic.Hooks (Component, component, JSX)
import Effect (Effect)
import React.Render.CustomBase (linearLayout, textView)
import Styles.Colors as Color
import ReactComponents.IncrementDecrement.Controller (Config, ComponentOutput(..), config, ComponentAction(..), eval)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, component, useEffect, useState)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console (log)
import Halogen.VDom.Types (VDom(..))
import Data.Maybe (Maybe(..), maybe)
-- import Helpers.Utils ((:<>))
-- import Prim.Boolean (False)

app :: (ComponentOutput -> Effect Unit) -> Component Config
app push = do 
  component "app" \initialState -> React.do
    -- count /\ setCount <- useState initialState.initialCount
    -- let onIncrement = setCount (\prevCount -> prevCount + 1)
    -- let onDecrement = setCount (\prevCount -> if(prevCount > 1) then prevCount - 1 else prevCount)
    state /\ updatedState <- useState initialState
    pure $ view push state updatedState
  
view :: (ComponentOutput -> Effect Unit) -> Config -> ((Config -> Config) -> Effect Unit) -> JSX
view push config updateState =
  linearLayout 
  { height: "wrap_content"
  , width: "match_parent"
  , orientation: "vertical"
  , weight: "0.0"
  , margin: config.margin
  } [ linearLayout 
    { height: "wrap_content"
    , width: "match_parent"
    , orientation: "horizontal"
    , padding: "4, 4, 4, 4"
    , cornerRadius: "8.0"
    , background: config.backgroundColor
    , stroke: config.stroke
    } [ textView 
        { background: config.minus.backgroundColor
        , text: "-"
        , textSize: config.fontSize
        , gravity: "center"
        , cornerRadius: config.cornerRadius
        , width: config.minus.width
        , padding: config.padding
        , onClick: eval Decrement updateState
        , height: config.minus.height
        , rippleColor: config.minus.rippleColor
        }
      , textView 
        { background: Color.white900
        , text: (show config.initialCount)
        , textSize: config.fontSize
        , height: "wrap_content"
        , color: Color.black800
        , padding: config.padding
        , weight: "1.0"
        , gravity: "center"
        } 
      , textView
        { background: config.plus.backgroundColor
        , width: config.plus.width
        , height: config.plus.height
        , text: "+"
        , textSize: config.fontSize
        , color: config.plus.textColor
        , padding: config.padding
        , cornerRadius: config.cornerRadius
        , onClick: eval Increment updateState
        , rippleColor: config.plus.rippleColor 
        }
      ]
    , linearLayout
      { width : "match_parent"
      , height : "wrap_content"
      , background : Color.black800
      , cornerRadius : config.cornerRadius
      , margin : config.margin
      , onClick: push $ Counter config.initialCount
      , gravity : "center"
      }[  textView
          { text : "Output"
          , color : Color.white900
          , padding : config.padding
          }
       ]
    ]