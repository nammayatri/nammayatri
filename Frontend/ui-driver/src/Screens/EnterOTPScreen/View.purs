{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.View where
import Data.Maybe (Maybe(..))
import Prelude (Unit, const, bind, pure, unit, ($), (<<<), (<>), (==), (>), discard,when,show, not, (/))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), alpha, background, clickable, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, text, textSize, textView, weight, width, afterRender, visibility, imageWithFallback,textFromHtml,frameLayout,lineHeight,cornerRadius,stroke, onChange)
import Components.OtpPrimaryEditText.View as OtpPrimaryEditText
import Components.OtpPrimaryEditText.Controller as OtpPrimaryEditTextController
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModel as StepsHeaderModel
import Animation.Config as AnimConfig
import Data.Maybe (fromMaybe)
import Data.Array (mapWithIndex)
import Effect (Effect)
import Styles.Colors as Color
import Screens.EnterOTPScreen.Controller (Action(..),eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types(STR(..))
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Helpers.Utils as HU
import Data.String as DS
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App
import Screens.EnterOTPScreen.ComponentConfig
import Data.Ring ((-))
import Storage (getValueToLocalStore, KeyStore(..))

screen :: ST.EnterOTPScreenState -> Screen Action ST.EnterOTPScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "EnterOTPScreen"
  , globalEvents : [ (\push -> do
                      _ <- pure $ HU.clearTimer ""
                      _ <- HU.startTimer 10 true push TIMERACTION
                      -- if (initialState.props.otpTmp) then
                      _ <- JB.onFocused TextChng initialState.data.editTextId
                      -- else
                      --   pure unit
                      --_ <- pure onChange push TextChng (EHC.getNewIDWithTag "EnterOTPNumberEditText")
                      pure (pure unit)) ] <> if (DS.length initialState.data.otp) > 0 then [] else [ HU.startOtpReciever AutoFill ]
  , eval
  }

-- view
--   :: forall w
--   . (Action -> Effect Unit)
--   -> ST.EnterOTPScreenState
--   -> PrestoDOM (Effect Unit) w
-- view push state =
--   linearLayout
--   [ height MATCH_PARENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   , afterRender (\action -> do
--         _ <- push action
--         _ <- JB.setFCMToken push $ SetToken
--         _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "EnterOTPScreenEditText")
--         pure unit
--         ) (const AfterRender)
--   , onBackPressed push (const BackPressed)
--   ][  linearLayout
--       [ width MATCH_PARENT
--       , weight 1.0
--       , orientation VERTICAL
--       ][  PrestoAnim.animationSet
--           [ Anim.fadeIn true
--           ] $ backArrow state push
--         , PrestoAnim.animationSet
--           [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
--           ] $ enterOTPTextView state
--         ,  primaryEditTextView state push
--       ]
--     , PrestoAnim.animationSet
--       [ Anim.fadeIn true
--       ] $ linearLayout
--           [ height WRAP_CONTENT
--           , width MATCH_PARENT
--           ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)]
--   ]

view
  :: forall w
  . (Action -> Effect Unit) -> ST.EnterOTPScreenState -> PrestoDOM (Effect Unit) w
view push state = let
  lang = getValueToLocalStore LANGUAGE_KEY
   in
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
        _ <- JB.setFCMToken push $ SetToken
        _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "EnterOTPScreenEditText")
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
          
          enterOTPView state lang push
          ]
      ]
    ]




-- --------------------- backArrow ----------------------------
-- backArrow :: ST.EnterOTPScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
-- backArrow state push =
--  linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , padding (Padding 16 16 16 0)
--   , margin (MarginTop 10)
--   ][  imageView
--       [ width (V 25)
--       , height (V 25)
--       , margin (MarginTop 20)
--       , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
--       , onClick push (const BackPressed)
--       ]
--   ]


-- ------------------------- enterOTPTextView -------------------
-- enterOTPTextView :: ST.EnterOTPScreenState -> forall w . PrestoDOM (Effect Unit) w
-- enterOTPTextView state=
--  textView
--   [ height WRAP_CONTENT
--   , width WRAP_CONTENT
--   , textSize FontSize.a_26
--   , text (getString ENTER_OTP)
--   , fontStyle $ FontStyle.bold LanguageStyle
--   , color Color.textPrimary
--   , margin (Margin 16 28 0 0)
--   ]


---------------------------- primaryEditTextView -------------------

-- primaryEditTextView :: ST.EnterOTPScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
-- primaryEditTextView state push =
--  linearLayout
--   [ width MATCH_PARENT
--   , height WRAP_CONTENT
--   , padding (Padding 18 18 20 0)
--   , orientation VERTICAL
--   ][  PrestoAnim.animationSet
--         [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
--         ] $ PrimaryEditText.view(push <<< PrimaryEditTextAction) ({
--         title: case (getValueToLocalStore LANGUAGE_KEY) of 
--                   "EN_US" -> (getString ENTER_OTP_SENT_TO) <> state.data.mobileNo
--                   _ -> state.data.mobileNo <> (getString ENTER_OTP_SENT_TO) ,
--         type: "number",
--         hint: (getString AUTO_READING_OTP),
--         text: state.data.capturedOtp,
--         isinValid: state.props.isValid,
--         error: Just (getString PLEASE_ENTER_VALID_OTP),
--         valueId: "EditTextOtp",
--         pattern : Just "[0-9]*,4",
--         fontSize : FontSize.a_18,
--         letterSpacing : PX if state.data.otp == "" then 0.0 else 5.0,
--         id : (EHC.getNewIDWithTag "EnterOTPScreenEditText")
--       })
--     , PrestoAnim.animationSet
--       [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
--       ] $ underlinedTextView state push
--   ]

--------------------------------- underlinedTextView ----------------------
-- underlinedTextView :: ST.EnterOTPScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
-- underlinedTextView state push =
--  linearLayout
--   [ width WRAP_CONTENT
--   , height WRAP_CONTENT
--   , margin (MarginTop 18)
--   , orientation VERTICAL
--   , onClick push (const ResendOTP)
--   ][  textView
--       [ height WRAP_CONTENT
--       , width WRAP_CONTENT
--       , text (getString RESEND_OTP)
--       , textSize FontSize.a_14
--       , fontStyle $ FontStyle.semiBold LanguageStyle
--       , color Color.mainPrimary
--       , visibility if state.props.resendEnabled then VISIBLE else GONE
--       ]
--     , textView
--       [ height WRAP_CONTENT
--       , width WRAP_CONTENT
--       , text $  (getString RESEND_OTP_IN) <> "  " <> state.data.timer
--       , textSize FontSize.a_14
--       , fontStyle $ FontStyle.semiBold LanguageStyle
--       , visibility if state.props.resendEnabled then GONE else VISIBLE
--       , color if state.props.resendEnabled then Color.mainPrimary else Color.black700
--       ]
--     , textView
--       [ height $ V 1
--       , width MATCH_PARENT
--       , background  Color.black700
--       , margin (Margin 1 0 2 0)
--       , visibility if state.props.resendEnabled then GONE else VISIBLE
--       ]
--     , textView
--       [ height $ V 1
--       , width MATCH_PARENT
--       , background Color.mainPrimary
--       , margin (Margin 1 0 2 0)
--       , visibility if state.props.resendEnabled then VISIBLE else GONE
--       ]
--   ]

enterOTPView :: ST.EnterOTPScreenState -> String -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
enterOTPView state lang push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][
      linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ][
      PrestoAnim.animationSet
      [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
      ] $ OtpPrimaryEditText.view (push <<< OtpPrimaryEditTextAction) (otpEditTextConfig state) 
    ] 
     --textBoxes push state
    ,  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , clickable state.props.resendEnabled
      ][linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , alpha if state.props.resendEnabled then 1.0 else 0.5
      ][  textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text ("Resend")
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.blue900
        , onClick push (const ResendOTP)
        ]
        , linearLayout
          [ width MATCH_PARENT
          , height (V 1)
          , background Color.blue900
          ][]
      ]
      , textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text if lang == "HI_IN" then ("  "<> show state.data.timer <> "s  "<> ("IN")) else ("  " <> ("IN") <> "  "<> show state.data.timer <> "  s")
        , textSize FontSize.a_12
        , lineHeight "22"
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.blue900
        , visibility if state.props.resendEnabled then GONE else VISIBLE
        ]]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , weight 1.0
      ][]
   -- , PrestoAnim.animationSet
  --    [ Anim.fadeIn state.props.enterOTP
   --   ] $
      ,linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][PrimaryButton.view (push <<< PrimaryButtonActionController) (verifyOTPButtonConfig state)]
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
    , onClick (\action -> do
                when isLink $ JB.openUrlInApp (fromMaybe "www.nammayatri.in" link)--"https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc/view?usp=sharing"
                pure unit
              ) (const TermsAndConditions)
    ]




-- otpView :: forall w . (Action -> Effect Unit) -> ST.EnterMobileNumberScreenState -> PrestoDOM (Effect Unit) w
-- otpView push state =
--    linearLayout
--       [ width MATCH_PARENT
--            , height WRAP_CONTENT
--            , margin (Margin 20 0 20 0)
--            , orientation VERTICAL
--        ]
--              ([] <> [textBoxes push state] <> [singleTextBox push state] <>
--                     [textView (
--                     [ width state.subHeadingConfig.width
--                     , height state.subHeadingConfig.height
--                     , color state.subHeadingConfig.color
--                     , text state.subHeadingConfig.text
--                     , visibility state.subHeadingConfig.visibility
--                     , fontStyle state.subHeadingConfig.fontStyle
--                     , textSize state.subHeadingConfig.fontSize
--                     , gravity state.subHeadingConfig.gravity
--                     , cornerRadius state.subHeadingConfig.cornerRadius
--                     , padding state.subHeadingConfig.padding
--                     , margin state.subHeadingConfig.margin
--                     , weight state.subHeadingConfig.weight
--                     ]
--                     )] <>
--                     [textView (
--                     [ width state.errorConfig.width
--                     , height state.errorConfig.width
--                     , visibility state.errorConfig.visibility
--                     , margin state.errorConfig.margin
--                     , text state.errorConfig.text
--                     , color state.errorConfig.color
--                     , textSize state.errorConfig.fontSize
--                     , gravity state.errorConfig.gravity
--                     , cornerRadius state.errorConfig.cornerRadius
--                     , padding state.errorConfig.padding
--                     , weight state.errorConfig.weight
--                     ] <> FontStyle.body1 TypoGraphy
--                   )])                 


textBoxes :: forall w . (Action -> Effect Unit) -> ST.EnterOTPScreenState -> PrestoDOM (Effect Unit) w
textBoxes push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity LEFT
  --, margin (Margin 0 20 0 20)
  , clickable false
  ](mapWithIndex (\index item ->
      textView
      [ width (V 48)
      , height (V 150)
      , color Color.blue600--Color.greyTextColor
      , text ( DS.take 1 (DS.drop index state.data.otp) )
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.bold LanguageStyle
      , gravity CENTER
      , cornerRadius 4.0
      , stroke ("1," <> if (state.props.isValid ) then Color.textDanger else if state.data.focusedIndex == index then Color.highlightBorderColor else Color.borderColorLight )
      --, margin (Margin ((EHC.screenWidth unit)/30) 0 ((EHC.screenWidth unit)/30) 0)
      --, onClick push (const (OnclickTextBox index))
      ]) [1,2,3,4])