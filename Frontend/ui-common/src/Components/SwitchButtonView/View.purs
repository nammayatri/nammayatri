{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SwitchButtonView.View where

import Prelude
import Effect (Effect)
import PrestoDOM 
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Font.Style as FontStyle
import Components.SwitchButtonView.Controller
import Common.Types.App (LazyCheck(..))

view :: forall w. (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  let
    backgroundColor = if config.isActive then config.backgroundColorOnActive else config.backgroundColorOnInActive

    align = if config.isActive then RIGHT else LEFT
  in
    linearLayout
      [ width $ V 40
      , height $ V 22
      , cornerRadius 100.0
      , background backgroundColor
      , stroke $ "1," <> backgroundColor
      , gravity CENTER_VERTICAL
      , onClick push $ const OnClick
      , clickable config.isClickable
      ]
      [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity align
          ]
          [ linearLayout
              [ width $ V 16
              , height $ V 16
              , background config.handleColor
              , cornerRadius 100.0
              , gravity CENTER_VERTICAL
              , margin (MarginHorizontal 2 2)
              ]
              []
          ]
      ]