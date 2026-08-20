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
import Prelude 
import PrestoDOM
import Screens.SplashScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (SplashScreenState)
import Styles.Colors as Color
import Helpers.Utils
import Engineering.Helpers.Commons
import RemoteConfig as RemoteConfig

screen :: SplashScreenState -> Screen Action SplashScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "SplashScreen"
  , globalEvents: [(\push -> pure $ push AfterRender)]
  , eval
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  SplashScreenState ->
  PrestoDOM (Effect Unit) w
view push _ =
  relativeLayout
    [ height $ V $ screenHeight unit
    , width $ V $ screenWidth unit
    , background "#111112"
    , gravity CENTER
    , clickable true
    ]
    [ 
      relativeLayout
      [ height $ V $ screenHeight unit
      , width $ V $ screenWidth unit
      ][
        imageView[
          height $ V $ screenHeight unit
        , width $ V $ screenWidth unit
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_splash_bg_trans"
        ]
      , linearLayout [
          height $ V $ screenHeight unit
        , width $ V $ screenWidth unit
        , gravity BOTTOM
        , padding $ PaddingBottom 50
        ][
          imageView[
            height $ V $ 48
          , width MATCH_PARENT
          , gravity CENTER
          , imageWithFallback $ fetchImage FF_ASSET "ic_powered_by"
          ]
        ]
      ]
    , lottieAnimationView
        [ height $ V $ screenHeight unit
        , width $ V $ screenWidth unit
        , id (getNewIDWithTag "splashLottieAnimation")
        , afterRender
            ( \action -> do
                let bundleLottieConfig = RemoteConfig.getBundleSplashConfig "lazy"
                void $ pure $ startLottieProcess lottieAnimationConfig { rawJson = bundleLottieConfig.lottieUrl, lottieId = (getNewIDWithTag "splashLottieAnimation"), speed = 1.8, cacheEnabled = false }
                push action
            )
            (const AfterRender)
        ]
    ]
