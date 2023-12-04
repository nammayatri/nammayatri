{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PopUpScreen.View where

import Prelude --(Unit, bind, const, map, pure, unit, ($), (&&), (<<<))
import PrestoDOM --(Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM,  Visibility(..), color, fontStyle, frameLayout, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, width)
import Effect (Effect)
import Language.Strings(getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Screens.PopUpScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Data.Maybe (Maybe(..))
import Components.RideAllocationModal as RideAllocationModal
import JBridge as JB
import Screens.PopUpScreen.ComponentConfig

screen :: ST.PopUpScreenState -> ScopedScreen Action ST.PopUpScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "PopUpScreen"
  , globalEvents : [(\push -> do
    _ <- JB.setStoreCallBackPopUp push PopUpCallBack
    pure $ pure unit)]
  , eval
  , parent : Nothing 
  }

view :: forall w. (Action -> Effect Unit) -> ST.PopUpScreenState -> PrestoDOM (Effect Unit) w
view push state = 
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackOpacity50
    , afterRender push (const AfterRender)
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ][ horizontalScrollView
           [ width MATCH_PARENT
           , height MATCH_PARENT
           , orientation HORIZONTAL
           ][  linearLayout
               [ width MATCH_PARENT
               , height MATCH_PARENT
               , orientation HORIZONTAL
               ]  (map (\(item) -> 
                  (if item.seconds == 0 then 
                    linearLayout[][]
                    else
                      RideAllocationModal.view (push <<< RideAllocationModalAction) ( rideAllocationModalConfig state item ))
               ) state.data.availableRides )
           ]
        ]
    ]