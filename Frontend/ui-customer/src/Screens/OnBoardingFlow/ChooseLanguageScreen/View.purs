{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ChooseLanguageScreen.View where

import Animation as Anim 
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import Components.MenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Effect (Effect)
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Prelude (Unit, bind, const, pure, unit, ($), (&&), (/=), (<<<),(<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, scrollView, text, textSize, textView, weight, width, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Screens.ChooseLanguageScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Screens.OnBoardingFlow.ChooseLanguageScreen.ComponentConfig 
import Helpers.Utils (fetchImage, FetchImageFrom(..))

screen :: ST.ChooseLanguageScreenState -> Screen Action ST.ChooseLanguageScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ChooseLanguageScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.ChooseLanguageScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout 
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  ][  linearLayout 
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin (Margin 0 10 0 24)
  , background Color.white900
  , padding $ Padding 16 EHC.safeMarginTop 16 EHC.safeMarginBottom
  , onBackPressed (\action -> do 
                    _ <- push action 
                    _ <- pure $ JB.minimizeApp ""
                    pure unit) (const BackPressed)
  , afterRender push (const AfterRender)
  ][  PrestoAnim.animationSet 
    [ Anim.removeYAnimFromTop $ removeYAnimFromTopConfig {ifAnim = state.props.exitAnimation}]--100 0 (-10) 0 1.0 0.0 state.props.exitAnimation $ PrestoAnim.Bezier 0.37 0.0 0.63 1.0 ]
      $ scrollLanguageView state push
    , PrestoAnim.animationSet 
        [ Anim.fadeIn $ true && EHC.os /= "IOS",
          Anim.fadeOut $ state.props.exitAnimation && EHC.os /= "IOS"
        ] $ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
  ]
]

scrollLanguageView :: forall w . ST.ChooseLanguageScreenState  -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
scrollLanguageView state push = 
  scrollView 
    [ height MATCH_PARENT 
    , width MATCH_PARENT
    , weight 1.0
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        ][linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER
          ][PrestoAnim.animationSet 
            [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig --300 10 0 0 true $ PrestoAnim.Bezier 0.37 0.0 0.63 1.0
            ] $ imageView
            [ width ( V 270)
            , height ( V 270)
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_welcome_customer"
            ]]
        , PrestoAnim.animationSet 
          [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig --300 10 0 0 true $ PrestoAnim.Bezier 0.37 0.0 0.63 1.0
          ] $ textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , textSize FontSize.a_26
            , text "Welcome to Namma Yatri"
            , color Color.greyTextColor
            , gravity CENTER_HORIZONTAL
            , fontStyle $ FontStyle.bold LanguageStyle
            ]
        ]
        , PrestoAnim.animationSet 
          [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig --300 10 0 0 true $ PrestoAnim.Bezier 0.37 0.0 0.63 1.0
          ] $ textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text "Choose language"
          , color Color.inactive
          , margin $ MarginTop 50
          ] <> FontStyle.subHeading2 TypoGraphy
        , listLanguageView state push
    ]
    ]

listLanguageView :: ST.ChooseLanguageScreenState -> (Action -> Effect Unit) -> forall w .  PrestoDOM (Effect Unit) w
listLanguageView state push = 
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](DA.mapWithIndex (\ index lang_data -> 
        PrestoAnim.animationSet 
          [ Anim.translateYAnimFromTopWithAlpha $ translateYAnimMapConfig index --{duration = 300 + (50 * index)} (300 + (50*index)) 10 0 0 true $ PrestoAnim.Bezier 0.37 0.0 0.63 1.0
          ] $ MenuButton.view (push <<< MenuButtonActionController) (menuButtonConfig state lang_data)) state.data.config.languageList)
