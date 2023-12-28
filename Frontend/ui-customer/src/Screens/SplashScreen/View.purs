{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SplashScreen.View where

import Animation (screenAnimation)
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Helpers.Utils (getAssetsBaseUrl)
import JBridge (lottieAnimationConfig, startLottieProcess)
import Prelude (Unit, const, discard, pure, void, ($), (<>))
import PrestoDOM (Gravity(..), Length(..), PrestoDOM, ScopedScreen, afterRender, background, clickable, gravity, height, id, linearLayout, lottieAnimationView, width)
import Screens.SplashScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (SplashScreenState)
import Styles.Colors as Color

screen :: SplashScreenState -> ScopedScreen Action SplashScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "SplashScreen"
  , globalEvents: []
  , eval
  , parent : Just "SplashScreen"
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  SplashScreenState ->
  PrestoDOM (Effect Unit) w
view push _ =
  screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.black900
        , gravity CENTER
        , clickable true
        ]
        [ lottieAnimationView
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , id (getNewIDWithTag "splashLottieAnimation")
            , afterRender
                ( \action -> do
                    void $ pure $ startLottieProcess lottieAnimationConfig { rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/splash_lottie.json", lottieId = (getNewIDWithTag "splashLottieAnimation"), speed = 1.8 }
                    push action
                )
                (const AfterRender)
            ]
        ]
