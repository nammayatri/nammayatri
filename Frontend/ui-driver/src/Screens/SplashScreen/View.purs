{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SplashScreen.View where

import Engineering.Helpers.Commons as EHC
import Prelude
import PrestoDOM
import Effect (Effect)
import Screens.SplashScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Log (printLog)
import Helpers.Utils as HU
import JBridge as JB
import Common.Types.App as CTA
import Data.Maybe (Maybe(..))
import RemoteConfig as RemoteConfig
import Animation as Anim
import PrestoDOM.Animation as PrestoAnim

screen :: ST.SplashScreenState -> LoggableScreen Action ST.SplashScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "SplashScreen"
  , globalEvents: globalEvents'
  , eval
  , parent : Nothing
  , logWhitelist : initialState.data.config.logWhitelistConfig.splashScreenLogWhitelist
  }
  where 
    globalEvents' = [
        (\push -> do
          let _ = printLog "SplashScreen " "view "
          pure $ push AfterRender)
      ]

view ::forall w. (Action -> Effect Unit) -> ST.SplashScreenState -> PrestoDOM (Effect Unit) w
view push _ =
  PrestoAnim.exitAnimationSetForward [ Anim.translateOutXForwardAnim true, Anim.fadeOut true]
  $ PrestoAnim.exitAnimationSetBackward [Anim.translateOutXBackwardAnim true, Anim.fadeOut true] 
  $ relativeLayout
    [ height $ MATCH_PARENT
    , width $ MATCH_PARENT
    , background "#111112"
    , gravity CENTER
    , clickable true
    ]
    [ 
      relativeLayout
      [ height $ MATCH_PARENT
      , width $ MATCH_PARENT
      ][
        imageView[
          height $ V $ EHC.screenHeight unit
        , width $ V $ EHC.screenWidth unit
        , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_splash_bg_trans"
        ]
      , linearLayout [
          height $ MATCH_PARENT
        , width $ MATCH_PARENT
        , gravity BOTTOM
        , padding $ PaddingBottom 50
        ][
          imageView[
            height $ V $ 48
          , width MATCH_PARENT
          , gravity CENTER
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ic_powered_by"
          ]
        ]
      ]
    , lottieAnimationView
        [ height $ V $ EHC.screenHeight unit
        , width $ V $ EHC.screenWidth unit
        , id $ EHC.getNewIDWithTag "splashLottieAnimation"
        , afterRender
            ( \action -> do
                let bundleSplashConfig = RemoteConfig.getBundleSplashConfig "lazy"
                void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig { 
                  rawJson = bundleSplashConfig.lottieUrl, 
                  lottieId = EHC.getNewIDWithTag "splashLottieAnimation", 
                  speed = 1.8,
                  cacheEnabled = false 
                }
                push action
            )
            (const AfterRender)
        ]
    ]
