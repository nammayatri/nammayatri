{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ChooseLanguageScreen.View where

import Common.Types.App (LazyCheck(..))
import Screens.ChooseLanguageScreen.ComponentConfig (primaryButtonViewConfig, menuButtonConfig)
import Animation as Anim
import Animation.Config as AnimConfig
import Components.PrimaryButton as PrimaryButton
import Components.SelectMenuButton as MenuButton
import Data.Array as DA
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PaymentPage (consumeBP)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, pure, unit, discard, ($), (<<<), (==), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, background, clickable, color, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.ChooseLanguageScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

screen :: ST.ChooseLanguageScreenState -> Screen Action ST.ChooseLanguageScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "ChooseLanguageScreen"
  , globalEvents: [ (\_ -> pure $ runEffectFn1 consumeBP unit) ]
  , eval
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  ST.ChooseLanguageScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , clickable true
    , gravity BOTTOM
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ PrestoAnim.animationSet
            [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
            ]
            $ scrollableView state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            ]
            [ PrestoAnim.animationSet
                [ Anim.fadeIn $ true
                ]
                $ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)
            ]
        ]
    ]

------------------------------ scrollableView ------------------------------
scrollableView :: ST.ChooseLanguageScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
scrollableView state push =
  scrollView
    [ width MATCH_PARENT
    , weight 1.0
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , gravity CENTER
            ]
            [ PrestoAnim.animationSet
                [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
                ]
                $ imageView
                    [ width (V 300)
                    , height (V 190)
                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_welcome"
                    ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER_HORIZONTAL
            ]
            [ PrestoAnim.animationSet
                [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
                ]
                $ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString $ WELCOME_TEXT "WELCOME_TEXT"
                  , color Color.greyTextColor
                  , gravity CENTER_HORIZONTAL
                  , margin $ Margin 70 32 74 32
                  ]
                <> FontStyle.h1 TypoGraphy
            ]
        , PrestoAnim.animationSet
            [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
            ]
            $ textView
                ( [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString CHOOSE_LANGUAGE
                  , color Color.inactive
                  , margin $ Margin 16 0 0 0
                  ]
                    <> FontStyle.body1 TypoGraphy
                )
        , menuButtonDriver state push
        ]
    ]

----------------------------- menuButtonDriver ------------------------
menuButtonDriver :: ST.ChooseLanguageScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
menuButtonDriver state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 0 0 1 5
    , background Color.white900
    ]
    ( DA.mapWithIndex
        ( \index language ->
            PrestoAnim.animationSet
              [ Anim.translateYAnimFromTopWithAlpha $ AnimConfig.translateYAnimMapConfig index
              ]
              $ MenuButton.view
                  (push <<< MenuButtonAction)
                  (menuButtonConfig state index language)
        )
        (state.data.config.languageList)
    )
