module Screens.MeterFareScreen.View where

import Debug (spy)
import Effect (Effect)
import Prelude
import PrestoDOM
import Screens.MeterFareScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Helpers.Utils as HU
import Styles.Colors as Color
import Font.Style as FontStyle

screen :: ST.MeterFareScreenState -> Screen Action ST.MeterFareScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "MeterFareScreen"
  , globalEvents:
      [  \push -> do
            pure $ pure unit
      ]
  , eval:
      ( \action state -> do
          let _ = spy "MeterFareScreen ----- state" state
          let _ = spy "MeterFareScreen --------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.MeterFareScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , onBackPressed push (const $ BackPressed)
     ][linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , background Color.red100
        ][
        ]
      ,linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , background Colors.black800
        ][
        ]
      ,linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , background Color.red100
        ][
        ]
     ]
