module Screens.SplashScreen.View where

import Data.Int (round, toNumber)
import Engineering.Helpers.Commons as EHC
import Prelude (Unit, const, pure, unit, ($), (*))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, Screen, background, clickable, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, weight, width, afterRender, imageWithFallback)
import Effect (Effect)
import Screens.SplashScreen.Controller (Action(..), eval)
import Screens.Types as ST
import Styles.Colors as Color
import Log

screen :: ST.SplashScreenState -> Screen Action ST.SplashScreenState Unit
screen initialState =
  { initialState
  , view
  , name : "SplashScreen"
  , globalEvents : [(\push -> do
                      let _ = printLog "SplashScreen " "view "
                      pure (pure unit))]
  , eval
  }
view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.SplashScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.darkBlue
    , clickable true
    , onClick push (const (OnClick))
    , afterRender push (const AfterRender)
    ][ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , gravity CENTER
        , background Color.darkBlue
        , margin (Margin 0 0 0 0)
        , onClick push (const (OnClick))
        ][ imageView
            [ width MATCH_PARENT
            , height $ V $ round (0.41 * (toNumber (EHC.screenWidth unit)))
            , gravity CENTER
            , imageWithFallback ""
            ]
        ]
      , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.darkBlue
        , weight 1.0
        , gravity CENTER
        ][ imageView
            [ width ( V 210)
            , height ( V 120)
            , gravity CENTER
            , imageWithFallback ""
            ]
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , background Color.darkBlue
        , margin (Margin 0 0 0 25)
        ][ imageView
            [ width ( V 325)
            , height ( V 30)
            , imageWithFallback ""
            ]
        ]
    ]