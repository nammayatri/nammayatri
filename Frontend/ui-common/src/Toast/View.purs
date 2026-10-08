{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Toast.View where

import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Font.Style as FontStyle
import Toast.Controller (Action(..), ScreenOutput, eval)
import Toast.ScreenData (ToastState)
import Prelude (Unit, ($), (<>), (/=), discard, void, const, pure, unit)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, background, afterRender, cornerRadius, gravity, height, linearLayout, margin, orientation, padding, progressBar, stroke, text, textView, width, color, clickable)
import Styles.Colors as Color
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner)
import Types.App (defaultGlobalState)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Uncurried  (runEffectFn1)
import Effect.Class (liftEffect)
import Timers (startTimer)
import Engineering.Helpers.Commons as EHC
import ConfigProvider
import Debug (spy)

screen :: ToastState -> ScopedScreen Action ToastState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "Toast"
  , parent : Just "Toast"
  , globalEvents: [ (\_ -> do
       let _ = spy "Toast called" "Toast called"
       pure $ pure unit
       )
  ]
  , eval:
      \action state -> do
        let
          _ = spy "Toast action " action
        let
          _ = spy "Toast state " state
        eval action state
  }

view :: forall w .(Action -> Effect Unit) -> ToastState ->  PrestoDOM (Effect Unit) w
view push state =
  -- let config = (getAppConfig appConfig).alertConfig
  -- in
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black9000
    , gravity CENTER
    , clickable true
    , afterRender (\action -> do
                        void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ lift $ lift $ doAff do
                          liftEffect $ startTimer 2 "ToastId" "1" push TimerAction
                        push action
                  ) (const AfterRender)
    ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginHorizontal 40 40
    , orientation VERTICAL
    , background Color.white900
    , cornerRadius 10.0
    , gravity CENTER
    , padding $ PaddingVertical 20 20
    ][ textView $ 
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
      , text state.data.message
      , color Color.black700
      ] <> FontStyle.subHeading2 LanguageStyle
    -- , buttonView $
    --   [ height WRAP_CONTENT
    --   , width MATCH_PARENT
    --   , gravity CENTER
    --   , text "OK"
    --   , background Color.blue600
    --   , cornerRadius 5.0
    --   , color Color.white
    --   , padding $ PaddingHorizontal 16 16
    --   ] <> FontStyle.body LanguageStyle
    ]
  ]