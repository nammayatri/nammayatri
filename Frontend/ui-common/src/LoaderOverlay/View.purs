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
import Font.Style as FontStyle
import LoaderOverlay.Controller (Action, ScreenOutput, eval)
import LoaderOverlay.ScreenData (LoaderOverlayState)
import Prelude (Unit, ($), (<>), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, background, cornerRadius, gravity, height, linearLayout, margin, orientation, padding, progressBar, stroke, text, textView, width, color, clickable)
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import ConfigProvider

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
  let config = (getAppConfig appConfig).loaderConfig
  in
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black9000
    , gravity CENTER
    , clickable true
    ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginHorizontal 40 40
    , orientation VERTICAL
    , background Color.white900
    , cornerRadius 10.0
    , gravity CENTER
    , padding $ PaddingVertical 20 20
    ][ progressBar $
      [ height $ V 60
      , width $ V 60
      ] <> if EHC.os /= "IOS" then [stroke config.color] else []
    , textView $ 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , text state.data.title
      , color Color.black900
      ] <> FontStyle.h0 LanguageStyle
    , textView $ 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , text state.data.subTitle
      , color Color.black900
      ] <> FontStyle.subHeading2 LanguageStyle
    ]
  ]
