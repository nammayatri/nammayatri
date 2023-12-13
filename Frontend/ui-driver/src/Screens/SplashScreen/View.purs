{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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
  , name: "SplashScreen"
  , globalEvents:
      [ ( \push -> do
            let
              _ = printLog "SplashScreen " "view "
            pure (pure unit)
        )
      ]
  , eval
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  ST.SplashScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.darkBlue
    , clickable true
    , onClick push (const (OnClick))
    , afterRender push (const AfterRender)
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , gravity CENTER
        , background Color.darkBlue
        , margin (Margin 0 0 0 0)
        , onClick push (const (OnClick))
        ]
        [ imageView
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
        ]
        [ imageView
            [ width (V 210)
            , height (V 120)
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
        ]
        [ imageView
            [ width (V 325)
            , height (V 30)
            , imageWithFallback ""
            ]
        ]
    ]
