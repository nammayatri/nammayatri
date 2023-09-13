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
import Prelude (Unit, const, ($), (<<<), (<>), bind, pure , unit,(==),(>=))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), alpha, background, clickable, color, cornerRadius, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textView, visibility, weight, width, afterRender, imageWithFallback,textFromHtml,textSize,fontStyle)
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Screens.EnterMobileNumberScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types(STR(..))
import Data.Array (mapWithIndex)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App
import Screens.EnterMobileNumberScreen.ComponentConfig
import Components.StepsHeaderModel as StepsHeaderModel
import Animation.Config (translateYAnimConfig)
import Merchant.Utils (getValueFromConfig)

screen :: ST.EnterMobileNumberScreenState -> Screen Action ST.EnterMobileNumberScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "EnterMobileNumberScreen"
  , globalEvents : []
  , eval
  }

-- view
--   :: forall w
--   . (Action -> Effect Unit)
--   -> ST.EnterMobileNumberScreenState
--   -> PrestoDOM (Effect Unit) w
-- view push state =
--   linearLayout
--     [ height MATCH_PARENT
--     , width MATCH_PARENT
--     , orientation VERTICAL
--     , background Color.white900
--     , clickable true
--     , afterRender (\action -> do
--         _ <- push action
--         _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "EnterMobileNumberEditText")
--         pure unit
--         ) (const AfterRender)
--     , onBackPressed push (const BackPressed)
--     ][    PrestoAnim.animationSet
--           [ Anim.fadeIn true
--           ] $ backArrow state push
--         , PrestoAnim.animationSet
--           [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
--           ] $ enterMobileNumberTextView state
--         , PrestoAnim.animationSet
--           [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
--           ] $ primaryEditTextView state push
--         , PrestoAnim.animationSet
--           [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
--           ] $ termsAndConditionsView state push
--         , PrestoAnim.animationSet
--           [ Anim.fadeIn true
--           ] $ linearLayout
--               [ height WRAP_CONTENT
--               , width MATCH_PARENT
--               ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)]
--     ]


view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.EnterMobileNumberScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
   linearLayout
   [  height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
   ][  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender (\action -> do
        _ <- push action
        -- _ <- requestKeyboardShow (getNewIDWithTag "EnterMobileNumberEditText")
        pure unit
        ) (const AfterRender)
    , margin $ MarginBottom 24
    , padding (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom)
    , background Color.white900
    , onBackPressed push (const BackPressed)
    ][  PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (stepsHeaderModelConfig state)
      , frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , padding (Padding 16 0 16 0)
        ][
          -- PrestoAnim.animationSet
          --   [ Anim.fadeOut state.props.enterOTP
          --   , Anim.fadeIn  (not state.props.enterOTP)
          --   ] $
          enterMobileNumberView  state push
             
          ]
      ]
    ]
   


enterMobileNumberView:: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
enterMobileNumberView  state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility  VISIBLE--if state.props.enterOTP then GONE else VISIBLE
    , alpha 1.0 --if state.props.enterOTP then 0.0 else 1.0
    , orientation VERTICAL
    ][PrestoAnim.animationSet
      [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig -- 300 10 0 0 true PrestoAnim.Linear
      ] $ PrimaryEditText.view (push <<< PrimaryEditTextAction) (mobileNumberEditTextConfig state)
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , weight 1.0
      ][]
    , PrestoAnim.animationSet
      ( if EHC.os == "IOS" then [] else [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig -- 400 15 0 0 true PrestoAnim.Linear -- Temporary fix for iOS
      ]) $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin (Margin 0 0 0 10)
        ][ --commonTextView state "BY_TAPPING_CONTINUE" false Nothing push false--(getString BY_TAPPING_CONTINUE) 
       -- , commonTextView state " &nbsp; <u>T&Cs</u>" true (Just (getValueFromConfig "DOCUMENT_LINK")) push true
           underlinedTextView state push
          ]
     , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][PrimaryButton.view (push <<< PrimaryButtonActionController) (mobileNumberButtonConfig state)]
    ]

commonTextView :: ST.EnterMobileNumberScreenState -> String -> Boolean -> Maybe String -> (Action -> Effect Unit) -> Boolean -> forall w . PrestoDOM (Effect Unit) w
commonTextView state textValue isLink link push isTextFromHtml =
  textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , (if isTextFromHtml then textFromHtml else text) textValue
    , color if isLink then Color.blue900 else Color.black700
    , textSize FontSize.a_12
    , fontStyle $ FontStyle.medium LanguageStyle
    -- , onClick (\action -> do
    --             when isLink $ JB.openUrlInApp (fromMaybe "www.nammayatri.in" link)--"https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc/view?usp=sharing"
    --             pure unit
    --           ) (const TermsAndConditions)
    ]
--------------------- backArrow ----------------------------
-- backArrow :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
-- backArrow state push =
--  linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   , padding (Padding 16 16 16 0)
--   ][ imageView
--       [ width ( V 25 )
--       , height ( V 25 )
--       , margin (MarginTop 20)
--       , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
--       , onClick push (const BackPressed)
--       ]
--   ]

------------------------- enterMobileNumberTextView -------------------
-- enterMobileNumberTextView :: ST.EnterMobileNumberScreenState ->  forall w . PrestoDOM (Effect Unit) w
-- enterMobileNumberTextView state =
--  textView (
--   [ height WRAP_CONTENT
--   , width WRAP_CONTENT
--   , text (getString ENTER_MOBILE_NUMBER)
--   , color Color.textPrimary
--   , margin (Margin 16 37 0 0)
--   ] <> FontStyle.h1 TypoGraphy
--   )

-- ----------------------------- primaryEditTextView ---------------
-- primaryEditTextView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
-- primaryEditTextView state push =
--  linearLayout
--   [ height MATCH_PARENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   , weight 1.0
--   ][ linearLayout
--       [ width MATCH_PARENT
--       , height WRAP_CONTENT
--       , padding (Padding 20 0 20 0)
--       , margin (MarginTop 20)
--       ][  PrimaryEditText.view(push <<< PrimaryEditTextAction) ({
--           title: (getString MOBILE_NUMBER),
--           type: "number",
--           hint: (getString ENTER_MOBILE_NUMBER),
--           valueId: "MOBILE_NUMBER",
--           isinValid: state.props.isValid ,
--           error: Just (getString INVALID_MOBILE_NUMBER),
--           pattern : Just "[0-9]*,10",
--           text: "",
--           letterSpacing: PX 0.0,
--           id: (EHC.getNewIDWithTag "EnterMobileNumberEditText"),
--           fontSize : FontSize.a_18
--         })
--       ]
--   ]

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
    , text "By clicking Continue, you agree to our "--(getString CASE_TWO)
    , alpha 0.8
    , color Color.greyTextColor
    , textSize FontSize.a_14
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
        , textSize FontSize.a_14
        , text (" T&Cs")--getString NON_DISCLOUSER_AGREEMENT)
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
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin (MarginLeft 10)
      ][textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR)
        , color Color.greyTextColor
        , alpha 0.5
        ] <> FontStyle.body3 TypoGraphy)
      , underlinedTextView state push
      -- , textView (
      --   [ width WRAP_CONTENT
      --   , height WRAP_CONTENT
      --   , text (getString DATA_COLLECTION_AUTHORITY)
      --   , color Color.greyTextColor
      --   , alpha 0.5
      --   ] <> FontStyle.body3 TypoGraphy)
      ]
  ]
