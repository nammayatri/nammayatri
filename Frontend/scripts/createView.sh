#!/bin/bash
touch "$3/View.purs"
echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.$1Screen.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, background, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, weight, width,  textView, text, color, textSize, fontStyle)
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Prelude (Unit, const, ($), (<<<), (<>))
import Screens.$1Screen.Controller (Action(..), eval, ScreenOutput(..))
import Screens.$1Screen.ComponentConfig
import Font.Style as FontStyle

screen :: ST.$1ScreenState -> Screen Action ST.$1ScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : \"$1Screen\"
  , globalEvents : [] 
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> ST.$1ScreenState -> PrestoDOM (Effect Unit) w 
view push state = 
  Anim.screenAnimation 
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL 
      , gravity CENTER
      , onBackPressed push $ const BackPressed 
      , background Color.white900
      , padding $ Padding 16 16 16 16
      ][  GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , linearLayout 
          [ weight 1.0 
          , width MATCH_PARENT
          , gravity CENTER 
          ][  textView $ 
              [ text \"Dummy Text\"
              , color Color.black900
              ] <> FontStyle.h2 LanguageStyle
            ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT  
          , gravity BOTTOM
          , weight 1.0
          ][  PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
      ]" > $3/View.purs

echo "View.purs generated Successfully ! ------------------------------------------- ✔"