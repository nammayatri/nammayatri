{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.LoaderScreen.View where

import Animation as Anim
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons (storeHideLoaderCallback)
import Prelude (Unit, const, discard, pure, unit, ($))
import PrestoDOM (Length(..), Orientation(..), PrestoDOM, Screen, ScopedScreen, afterRender, background, height, linearLayout, onBackPressed, orientation, width)
import Screens.LoaderScreen.Controller (Action(..), ScreenOutput, eval)
import Styles.Colors as Color

screen :: forall a. a  -> ScopedScreen Action a ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "LoaderOverlay"
  , parent : Just "LoaderOverlay"
  , globalEvents: [(\push -> do
    runEffectFn2 storeHideLoaderCallback push BackPressed
    pure $ pure unit
    )]
  , eval
  }

view :: forall w a.(Action -> Effect Unit) -> a ->  PrestoDOM (Effect Unit) w
view push _ =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black9000

    ][ ]
