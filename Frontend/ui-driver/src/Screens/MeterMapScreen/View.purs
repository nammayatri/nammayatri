module Screens.MeterMapScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Prelude
import PrestoDOM
import Screens.MeterMapScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import JBridge (reallocateMapFragment, showMap)
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App as CTA
import Engineering.Helpers.Commons as EHC
import PrestoDOM.Properties (cornerRadii)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import Engineering.Helpers.Commons (getNewIDWithTag, safeMarginBottom, safeMarginTop)
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.RateCard as RateCard
import Screens.BookingOptionsScreen.ComponentConfig as BOP
import Components.InAppKeyboardModal as InAppKeyboardModal
import Data.String as DS
import Animation (screenAnimationFadeInOut)
import Data.String.CodeUnits (charAt)
import Language.Strings (getString)

screen :: ST.MeterMapScreenState -> Screen Action ST.MeterMapScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "MeterMapScreen"
  , globalEvents:
      [  \_ -> do
            _ <- reallocateMapFragment (EHC.getNewIDWithTag "MeterMapScreenMap")
            pure $ pure unit
      ]
  , eval:
      ( \action state -> do
          let _ = spy "MeterMapScreen ----- state" state
          let _ = spy "MeterMapScreen --------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.MeterMapScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut $ relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , onBackPressed push (const $ BackPressed)
  , afterRender
    (\action -> do
       _ <- push action
       _ <- showMap (EHC.getNewIDWithTag "MeterMapScreenMap") true "satellite" (17.0) 0.0 0.0 push ShowMap
       pure unit
    ) (const NoAction)
  ]([linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      , orientation VERTICAL
      , background Color.white900
      , cornerRadius 50.0
     ][ frameLayout 
       [ width MATCH_PARENT
       , height WRAP_CONTENT
       ] [googleMap state
         ,confirmPickupView push state
         ,if state.props.enableOtpModal then enterOtpModal push state else textView[height $ V 0, width $ V 0]
         ]
     ]
  ] <> if state.props.showRateCard then [ rateCardView push state ] else []
  )

enterOtpModal :: forall w . (Action -> Effect Unit) -> ST.MeterMapScreenState -> PrestoDOM (Effect Unit) w
enterOtpModal push state =
  InAppKeyboardModal.view (push <<< InAppKeyboardModalOtp) (enterOtpState state)

enterOtpState :: ST.MeterMapScreenState -> InAppKeyboardModal.InAppKeyboardModalState
enterOtpState state =
  let 
    config' = InAppKeyboardModal.config
    inAppModalConfig' = config'{
      modalType = ST.OTP,
      showResendOtpButton = true,
      otpIncorrect = if (state.props.otpAttemptsExceeded) then false else (state.props.otpIncorrect),
      otpAttemptsExceeded = (state.props.otpAttemptsExceeded),
    inputTextConfig {
      text = state.props.alternateMobileOtp,
      focusIndex = state.props.enterOtpFocusIndex
    },
    headingConfig {
      text = getString (ENTER_OTP)
    },
    errorConfig {
      text = if (state.props.otpIncorrect) then ((getString WRONG_OTP) ) else ""
    , visibility = if (state.props.otpIncorrect || state.props.otpAttemptsExceeded) then VISIBLE else GONE,
    margin = (Margin 0 0 0 8)
    },
    subHeadingConfig {
      text =  getString (OTP_SENT_TO) <> state.props.customerMobileNumber,
      color = Color.black800,
      margin = (Margin 0 0 0 8),
      visibility = (if not state.props.otpIncorrect then VISIBLE else GONE)
    },
    imageConfig {
        alpha = if (DS.length state.props.alternateMobileOtp < 4 || state.props.otpIncorrect) then 0.3 else 1.0
    },
    enableDeviceKeyboard = true
    }
  in inAppModalConfig'

underlinedTextView :: String -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
underlinedTextView value push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text value
          , color Color.primaryBlue
          , gravity CENTER
          , onClick push $ const $ ShowRateCard
          ]
        <> FontStyle.subHeading1 CTA.TypoGraphy
    , linearLayout
        [ width $ MATCH_PARENT
        , height $ V 1
        , background Color.primaryBlue
        , gravity CENTER
        ]
        []
    ] 
confirmPickupView :: forall w. (Action -> Effect Unit) -> ST.MeterMapScreenState -> PrestoDOM (Effect Unit) w
confirmPickupView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , layoutGravity "bottom"
  , orientation VERTICAL
  , background Color.primaryBlue
  , cornerRadii $ Corners 20.0 true true false false
  ][linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.primaryBlue
    , layoutGravity "bottom"
    , orientation HORIZONTAL
    , gravity CENTER
    , cornerRadii $ Corners 20.0 true true false false
    , padding $ Padding 5 5 5 5
    ][ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text "Earn  ₹100 when a new customer rides! 💸 "
      , color Color.white900
      , gravity CENTER
      , margin $ MarginRight 4
      ] <> (FontStyle.body1 CTA.TypoGraphy)
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text " | "
      , color Color.white900
      , gravity CENTER
      , margin $ MarginRight 4
      ] <> (FontStyle.body1 CTA.TypoGraphy)
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text " Learn more"
        , color Color.white900
        ] <> (FontStyle.body1 CTA.TypoGraphy)
      , linearLayout
        [ width $ MATCH_PARENT
        , height $ V 1
        , background Color.white900
        ]
      []
    ]
  ],
  linearLayout [
      width MATCH_PARENT
    , height  WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , cornerRadii $ Corners 20.0 true true false false 
    , padding $ Padding 16 16 16 16
    ][textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text  "Government Set Fare"
      , color Color.black800
      , gravity CENTER
      , weight 10.0
      , margin $ MarginBottom 8
      , fontStyle $ FontStyle.bold CTA.LanguageStyle
      ] <> (FontStyle.h2 CTA.TypoGraphy)
      ,linearLayout [
          width MATCH_PARENT
        , height  MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding $ Padding 50 10 50 10
        , margin $ Margin 10 10 10 10
        , gravity CENTER
        , cornerRadius 8.0
        , stroke ("1," <> "#E5E5E5")
      ][
        textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text  "₹ 315.0"
        , color Color.black800
        , gravity CENTER
        , weight 1.0
        , margin $ MarginBottom 8
        ] <> (FontStyle.heading CTA.TypoGraphy)
        ,textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text  "Distance: 12.2 km"
        , color Color.black800
        , gravity CENTER
        , weight 1.0
        , margin $ MarginBottom 8
        ] <> (FontStyle.h2 CTA.TypoGraphy)
        , underlinedTextView "View Rate Card" push
      ]
      ,linearLayout [
          width MATCH_PARENT
        , height  WRAP_CONTENT
        , orientation VERTICAL
        , background Color.white900
      ][
        textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text  "Enter customer mobile number"
        , color Color.black800
        , gravity LEFT
        , margin $ MarginLeft 5
        , weight 1.0
        , textSize FontSize.a_12
        ] 
      , editText [ height MATCH_PARENT
        , width $ MATCH_PARENT
        , padding $ Padding 12 12 12 12
        , margin $ Margin 5 5 5 5
        , background Color.white900
        , color Color.black 
        , hint "9931239812"
        , hintColor  "#A7A7A7"
        , weight 1.0
        , pattern "[0-9]*,10"
        , singleLine false 
        , stroke ("1," <> "#E5E5E5")
        , textSize FontSize.a_16
        , onChange push PhoneNoChanged
        ]
      ,textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 5 5 5 20
        , text  "Invalid mobile number!"
        , color Color.red900
        , gravity LEFT
        , margin $ MarginLeft 5
        , weight 1.0
        , textSize FontSize.a_12
        , visibility case charAt 0 state.props.customerMobileNumber of
                        Just '0' -> VISIBLE
                        Just '1' -> VISIBLE
                        Just '2' -> VISIBLE
                        Just '3' -> VISIBLE
                        Just '4' -> VISIBLE
                        Just '5' -> if state.props.customerMobileNumber == "5000500050" then GONE else VISIBLE
                        _ -> GONE
        ]
        , primaryButtonView state push
      ]
    ]
  ] 

googleMap :: forall w . ST.MeterMapScreenState -> PrestoDOM (Effect Unit) w
googleMap _ =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  , id (EHC.getNewIDWithTag "MeterMapScreenMap")
  ][]

rateCardView :: forall w. (Action -> Effect Unit) -> ST.MeterMapScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RateCard.view (push <<< RateCardAction) (BOP.rateCardConfig state.data.rateCard state.data.appConfig.rateCardScreen.showTollCharges state.data.appConfig.rateCardScreen.showDriverAdditions) ]


primaryButtonConfig :: ST.MeterMapScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = "Confirm Ride"
        , color =  "#FCC32C"
        , height = V $ 30
        }
      , height = V $ 50
      , width = MATCH_PARENT
      , padding = Padding 0 0 0 0
      , gravity = CENTER
      , cornerRadius = 8.0
      , background =  "#2C2F3A"
      , margin = (MarginHorizontal 5 5)
      , isClickable = state.props.isCustomerNumberValid
      , alpha = if state.props.isCustomerNumberValid then 1.0 else 0.3
      , id = "ConfirmRideMeterMap"
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in primaryButtonConfig'

primaryButtonView :: forall w. ST.MeterMapScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
primaryButtonView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    , cornerRadius  8.0
    ][ PrimaryButton.view (push <<< PrimaryButtonAC)(primaryButtonConfig state)]
