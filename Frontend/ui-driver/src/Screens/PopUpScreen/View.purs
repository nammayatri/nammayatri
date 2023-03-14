module Screens.PopUpScreen.View where

import Prelude --(Unit, bind, const, map, pure, unit, ($), (&&), (<<<))
import PrestoDOM --(Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), color, fontStyle, frameLayout, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, width)
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
  , parent : Just "PopUpScreen"
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