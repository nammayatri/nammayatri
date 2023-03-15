{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.GenericMessageModal.View where

import Prelude
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, clickable, gravity, height, textView, linearLayout, margin, onClick, orientation, padding, width, text, color, weight, textSize, fontStyle)
import Components.PrimaryButton.View as PrimaryButton
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.GenericMessageModal.Controller(Action(..), Config)
import Styles.Colors as Color
import Language.Types (STR(..))
import Font.Style as FontStyle
import Font.Size as FontSize
import PrestoDOM.Properties(cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Common.Types.App

view :: forall w. (Action -> Effect Unit) -> Config ->  PrestoDOM (Effect Unit) w
view push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity BOTTOM
    , orientation VERTICAL
    , background Color.black9000
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        , background Color.white900
        , cornerRadii $ Corners 20.0 true true false false
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , orientation VERTICAL
            , padding (Padding 40 30 40 25)
            ][  textView (
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text state.text
                , color Color.black700
                , gravity CENTER
                , weight 1.0
                , textSize FontSize.a_16
                , fontStyle $ FontStyle.regular LanguageStyle
                , padding (PaddingBottom 40)
                , margin (MarginHorizontal 10 10)
                ])
                , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , gravity BOTTOM
                  ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
            ]
        ]
            
    ]

primaryButtonConfig :: Config -> PrimaryButtonConfig.Config 
primaryButtonConfig state = let 
    config = PrimaryButtonConfig.config
    primaryButtonConfig' = config
      { textConfig
      { text = state.buttonText
      , color = Color.primaryButtonColor
      , textSize = FontSize.a_18}
      , background = Color.black900
      , cornerRadius = 0.0
      , height = (V 50)
      }
  in primaryButtonConfig'