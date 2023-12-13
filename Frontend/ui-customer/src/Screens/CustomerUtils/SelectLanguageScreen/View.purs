{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SelectLanguageScreen.View where

import Screens.CustomerUtils.SelectLanguageScreen.ComponentConfig
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.MenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Prelude (Unit, const, map, ($), (<<<), (==), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, background, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, weight, width)
import Screens.SelectLanguageScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color

screen :: ST.SelectLanguageScreenState -> Screen Action ST.SelectLanguageScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "SelectLanguageScreen"
  , globalEvents: []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> ST.SelectLanguageScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , background Color.white900
        , padding (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 then 24 else EHC.safeMarginBottom))
        , afterRender push (const AfterRender)
        ]
        [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
        , if (not state.data.config.nyBrandingVisibility) then
            linearLayout
              [ height $ V 1
              , width MATCH_PARENT
              , background Color.greySmoke
              ]
              []
          else
            linearLayout [] []
        , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding (Padding 16 0 16 0)
            ]
            [ listLanguageView state push
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity BOTTOM
                , weight 1.0
                ]
                [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state) ]
            ]
        ]

listLanguageView :: forall w. ST.SelectLanguageScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
listLanguageView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    (map (\lang_data -> MenuButton.view (push <<< MenuButtonActionController) (menuButtonConfig state lang_data)) (state.data.config.languageList))
