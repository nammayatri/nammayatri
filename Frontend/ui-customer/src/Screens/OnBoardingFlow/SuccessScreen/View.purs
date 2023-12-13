{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SuccessScreen.View where

import Prelude
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..))
import Control.Monad.Except.Trans (lift, runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl)
import JBridge (startLottieProcess, lottieAnimationConfig)
import Presto.Core.Types.Language.Flow (delay, doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, afterRender, background, color, gravity, height, id, linearLayout, lottieAnimationView, margin, orientation, padding, text, textView, width, imageView, imageUrl)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.SuccessScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (SuccessScreenState)
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: SuccessScreenState -> ScopedScreen Action SuccessScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "SuccessScreen"
  , parent: Just "SuccessScreen"
  , globalEvents:
      [ ( \push -> do
            _ <-
              launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
                $ do
                    lift $ lift $ void $ delay $ Milliseconds 3500.0
                    lift $ lift $ doAff do liftEffect $ push CountDown
                    pure unit
            pure $ pure unit
        )
      ]
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> SuccessScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , background Color.black9000
    , padding (PaddingHorizontal 24 24)
    , afterRender push (const AfterRender)
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER_HORIZONTAL
        , background Color.white900
        , cornerRadii $ Corners 16.0 true true true true
        , padding (Padding 16 24 16 24)
        ]
        [ if EHC.os == "IOS" && (getValueToLocalStore VERSION_NAME == "1.2.4") then
            imageView
              [ height $ V 160
              , width $ V 280
              , imageUrl $ fetchImage FF_ASSET "ny_ic_success_lottie_placeholder"
              ]
          else
            lottieLoaderView state push
        , textView
            $ [ width MATCH_PARENT
              , height WRAP_CONTENT
              , margin $ MarginVertical 0 4
              , text state.title
              , gravity CENTER
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER
              , text state.subTitle
              , color Color.black700
              ]
            <> FontStyle.subHeading2 TypoGraphy
        ]
    ]

lottieLoaderView :: forall w. SuccessScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lottieLoaderView state push =
  lottieAnimationView
    [ id (getNewIDWithTag "SuccessLottieView")
    , afterRender
        ( \action ->
            void $ pure $ startLottieProcess lottieAnimationConfig { rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/success_lottie.json", lottieId = (EHC.getNewIDWithTag "SuccessLottieView"), speed = 1.0 }
        )
        (const CountDown)
    , height MATCH_PARENT
    , width WRAP_CONTENT
    ]
