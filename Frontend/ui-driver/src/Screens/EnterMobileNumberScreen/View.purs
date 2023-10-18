{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.View where

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, ($), (<<<), (<>), bind, pure , unit)
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), alpha, background, clickable, color, cornerRadius, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textView, visibility, weight, width, afterRender, imageWithFallback)
import Components.PrimaryEditText.Views as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Screens.EnterMobileNumberScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types(STR(..))
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App
import Screens.EnterMobileNumberScreen.ComponentConfig
import MerchantConfig.Utils (getValueFromConfig)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

screen :: ST.EnterMobileNumberScreenState -> Screen Action ST.EnterMobileNumberScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "EnterMobileNumberScreen"
  , globalEvents : []
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.EnterMobileNumberScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , clickable true
    , afterRender (\action -> do
        _ <- push action
        _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "EnterMobileNumberEditText")
        pure unit
        ) (const AfterRender)
    , onBackPressed push (const BackPressed)
    ][    PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ backArrow state push
        , PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ enterMobileNumberTextView state
        , PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ primaryEditTextView state push
        , PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ termsAndConditionsView state push
        , PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)]
    ]


--------------------- backArrow ----------------------------
backArrow :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
backArrow state push =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 16 16 16 0)
  ][ imageView
      [ width ( V 25 )
      , height ( V 25 )
      , margin (MarginTop 20)
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
      , onClick push (const BackPressed)
      ]
  ]

------------------------- enterMobileNumberTextView -------------------
enterMobileNumberTextView :: ST.EnterMobileNumberScreenState ->  forall w . PrestoDOM (Effect Unit) w
enterMobileNumberTextView state =
 textView (
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , text (getString ENTER_MOBILE_NUMBER)
  , color Color.textPrimary
  , margin (Margin 16 37 0 0)
  ] <> FontStyle.h1 TypoGraphy
  )

----------------------------- primaryEditTextView ---------------
primaryEditTextView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
primaryEditTextView state push =
 linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , weight 1.0
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding (Padding 20 0 20 0)
      , margin (MarginTop 20)
      ][  PrimaryEditText.view(push <<< PrimaryEditTextAction) ({
          title: (getString MOBILE_NUMBER),
          type: "number",
          hint: (getString ENTER_MOBILE_NUMBER),
          valueId: "MOBILE_NUMBER",
          isinValid: state.props.isValid ,
          error: Just (getString INVALID_MOBILE_NUMBER),
          pattern : Just "[0-9]*,10",
          text: "",
          letterSpacing: PX 0.0,
          id: (EHC.getNewIDWithTag "EnterMobileNumberEditText"),
          fontSize : FontSize.a_18
        })
      ]
  ]

--------------------------------- underlinedTextView ----------------------
underlinedTextView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
underlinedTextView state push =
 linearLayout
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 , orientation HORIZONTAL
 ][ textView (
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text (getString CASE_TWO)
    , alpha 0.5
    , color Color.greyTextColor
    ] <> FontStyle.body3 TypoGraphy),
    linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , onClick (\action -> do
                  _<- push action
                  _ <- JB.openUrlInApp $ getValueFromConfig "DOCUMENT_LINK" 
                  pure unit
                  ) (const NonDisclosureAgreementAction)
      ][ textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString NON_DISCLOUSER_AGREEMENT)
        , color Color.primaryBlue
        ] <> FontStyle.body3 TypoGraphy)
      ]

 ]

-------------------------------- termsAndConditionsView ------------------
termsAndConditionsView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
termsAndConditionsView state push =
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin (Margin 15 10 16 20)
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginLeft 10)
      , gravity CENTER
      ][textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ (getString BY_CLICKING_CONTINUE_YOU_WILL_BE_AGREEING_TO_OUR) <> " "
        , color Color.greyTextColor
        , alpha 0.5
        ] <> FontStyle.body3 TypoGraphy)
      , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , onClick (\action -> do
                  _<- push action
                  _ <- JB.openUrlInApp $ getValueFromConfig "DOCUMENT_LINK" 
                  pure unit
                  ) (const NonDisclosureAgreementAction)
      ][ textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "T&Cs"
        , color Color.primaryBlue
        ] <> FontStyle.body3 TypoGraphy)
      ]
      ]
  ]
