{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module LoaderOverlay.View where

import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Style as FontStyle
import JBridge (startLottieProcess)
import LoaderOverlay.Controller (Action(..), ScreenOutput, eval)
import LoaderOverlay.ScreenData (LoaderOverlayState)
import Prelude (Unit, const, pure, unit, ($), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, afterRender, background, cornerRadius, gravity, height, id, linearLayout, lottieAnimationView, margin, orientation, padding, text, textView, width, progressBar, progressColor)
import Styles.Colors as Color
import Helpers.Utils (getAssetsBaseUrl)

screen :: LoaderOverlayState -> ScopedScreen Action LoaderOverlayState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "LoaderOverlay"
  , parent : Just "LoaderOverlay"
  , globalEvents: []
  , eval
  }

view :: forall w .(Action -> Effect Unit) -> LoaderOverlayState ->  PrestoDOM (Effect Unit) w
view _ state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black9000
    , gravity CENTER
    ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginHorizontal 40 40
    , orientation VERTICAL
    , background Color.white900
    , cornerRadius 10.0
    , gravity CENTER
    , padding $ PaddingVertical 20 20
    ][ lottieAnimationView
      [ height $ V 100
      , width $ V 100
      , id (getNewIDWithTag "lottieAnimationLoaderOverlay")
      , afterRender (\_ -> pure $ startLottieProcess ((getAssetsBaseUrl FunctionCall) <> "lottie/ny_ic_loader.json") (getNewIDWithTag "lottieAnimationLoaderOverlay") true 0.6 "CENTER_CROP") (const unit)
      ]
    , textView $ 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text state.data.title
      ] <> FontStyle.h0 LanguageStyle
    , textView $ 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text state.data.subTitle
      ] <> FontStyle.subHeading2 LanguageStyle
    ]
  ]
