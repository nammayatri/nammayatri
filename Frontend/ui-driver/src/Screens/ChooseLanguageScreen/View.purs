module Screens.ChooseLanguageScreen.View where

import Prelude (Unit, const, unit, discard, ($), (<<<), (==), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, background, clickable, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, scrollView, text, textSize, textView, weight, width, afterRender, layoutGravity, padding, imageWithFallback)
import Components.SelectMenuButton.View as MenuButton
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Screens.ChooseLanguageScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types(STR(..))
import Font.Size as FontSize
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Data.Array as DA
import Common.Types.App
import Screens.ChooseLanguageScreen.ComponentConfig

screen :: ST.ChooseLanguageScreenState -> Screen Action ST.ChooseLanguageScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ChooseLanguageScreen" 
  , globalEvents : []
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.ChooseLanguageScreenState
  -> PrestoDOM (Effect Unit) w
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
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][ PrestoAnim.animationSet 
            [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
            ] $ scrollableView state push
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            ][PrestoAnim.animationSet 
              [ Anim.fadeIn $ true
              ] $ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)]
        ]
    ]

------------------------------ scrollableView ------------------------------
scrollableView :: ST.ChooseLanguageScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
scrollableView state push = 
 scrollView
  [ width MATCH_PARENT
  , weight 1.0
  ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        ][PrestoAnim.animationSet 
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ imageView
              [ width ( V 270)
              , height ( V 270)
              , imageWithFallback  "ny_ic_welcome,https://assets.juspay.in/nammayatri/images/driver/ny_ic_welcome.png"
              ]]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER_HORIZONTAL
          ][ PrestoAnim.animationSet 
            [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
            ] $ textView (
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , textSize FontSize.a_26
                , text "Welcome to Namma Yatri"
                , color Color.greyTextColor
                , gravity CENTER_HORIZONTAL
                ] <> FontStyle.h1 TypoGraphy
                )
        , PrestoAnim.animationSet 
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ textView (
              [ height WRAP_CONTENT
              , gravity CENTER_HORIZONTAL
              , width WRAP_CONTENT
              , textSize FontSize.a_26
              , text "Driver"
              , color Color.greyTextColor
              , margin (MarginTop 5)
              ] <> FontStyle.h1 TypoGraphy
            )
          ]
        , PrestoAnim.animationSet 
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig 
          ] $ textView (
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , textSize FontSize.a_17
              , text "Choose Language"
              , color Color.inactive
              , margin $ Margin 20 50 0 0
              ] <> FontStyle.body1 TypoGraphy
              )
        , menuButtonDriver state push
      ]
  ]

----------------------------- menuButtonDriver ------------------------
menuButtonDriver :: ST.ChooseLanguageScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
menuButtonDriver state push =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 0 0 1 5
  , background Color.white900
  ](DA.mapWithIndex
      (\ index language ->
      PrestoAnim.animationSet 
      [ Anim.translateYAnimFromTopWithAlpha $ AnimConfig.translateYAnimMapConfig index
      ] $ MenuButton.view
          (push <<< (MenuButtonAction))
          { text: {name: language.name, value: language.value, subtitle: language.subtitle}, isSelected: (state.props.selectedLanguage == language.value), index : index }) state.data.languages
  )