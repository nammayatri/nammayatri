module Screens.RideBookingFlow.MeterRideScreen.View where

import Prelude
import Screens.Types as ST
import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (<<<), (<>), (==), (>), not, void, discard, (-), show, (*), (<=), (>=), (/))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, relativeLayout, gravity, height, alpha, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, singleLine, id, frameLayout, scrollBarY, fillViewport, onAnimationEnd, rippleColor, alignParentBottom, progressBar, letterSpacing, editText, hint, hintColor, pattern, clickable, adjustViewWithKeyboard, onChange, onFocus, inputType, progressBarColor)
import Screens.RideBookingFlow.MeterRideScreen.Controller (Action(..), eval, ScreenOutput)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import PrestoDOM.Types.Core (LetterSpacing(..))
import Mobility.Prelude (boolToVisibility)
import Screens.Types as ST
import Styles.Colors as Color
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Properties (cornerRadii)
import JBridge as JB
import Debug
import Common.Types.App as CT
import Font.Style as FontStyle
import Engineering.Helpers.Commons as EHC
import Data.String as DS

screen :: ST.MeterRideScreenState -> Screen Action ST.MeterRideScreenState ScreenOutput
screen initialState =
    { initialState
    , view: view
    , name: "MeterRideScreen"
    , globalEvents: []
    , eval:
        ( \state action -> do
            let _ = spy "MeterRideScreen state" state
            let _ = spy "MeterRideScreen action" action
            eval state action
        ) 
    }

view :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
view push state = 
    Anim.screenAnimation $ relativeLayout
    [
        width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.white900
        , onBackPressed push $ const BackPressed
        , adjustViewWithKeyboard "true"
        , onClick push $ const ClearFocus
    ] $ 
    [
        meterRideView push state
    ] <> if state.props.showInfoCard then [infoView push state] else []

meterRideView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
meterRideView push state = 
    linearLayout
    [
        width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
    ]
    [
        topBarView push state
        , imageView [
        imageWithFallback $ fetchImage FF_ASSET "ny_ic_meterride_loading"
        , height $ V 200
        , width MATCH_PARENT
        ]
        , linearLayout[
            width MATCH_PARENT
            , height WRAP_CONTENT
        ]
        [
            textView $ [
            height WRAP_CONTENT
            , weight 1.0
            , gravity CENTER
            , text "Enter Ride OTP"
            , color Color.black600
            , letterSpacing $ PX 2.0
            ] <> FontStyle.h3 CT.TypoGraphy
        ]
        , otpView push state
        , if state.props.invalidOTP then invalidOTPView push state else linearLayout[width MATCH_PARENT, height $ V 52][]
        , enterButtonView push state
    ]

topBarView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
topBarView push state = 
    linearLayout 
    [
        width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
    ]
    [
        imageView [
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , height $ V 24
        , width $ V 24
        , margin $ Margin 16 16 16 16 
        ]

        , textView $ [
        height WRAP_CONTENT
        , weight 1.0
        , text "Meter Ride"
        , color Color.black900
        ] <> FontStyle.h3 CT.TypoGraphy
        
        , linearLayout [
        width WRAP_CONTENT
        , height WRAP_CONTENT
        , margin $ Margin 16 16 16 16 
        , cornerRadius 34.0
        , background Color.blue600
        , padding $ Padding 10 6 10 6
        , onClick push $ const ShowRateCard
        ]
        [
            textView $ [
            height WRAP_CONTENT
            , width WRAP_CONTENT
            , text "What's this?"
            , color Color.blue900
            ] <> FontStyle.h3 CT.TypoGraphy
        ]
    ]

otpView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
otpView push state = 
    linearLayout [
        width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.blue600
        , cornerRadius 60.0
        , gravity CENTER
        , margin $ Margin 24 32 24 0
        , padding $ Padding 40 20 40 20
    ]
    [
        linearLayout [
            width MATCH_PARENT
            , height WRAP_CONTENT
            , background Color.white900
            , gravity CENTER
            , cornerRadius 8.0
            , margin $ MarginRight 5
        ]
        [
            editText $ 
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , color Color.black900
            , onChange push $ OTPTextChanged1
            , onFocus push $ OTPFocussed1
            , gravity CENTER
            , cornerRadius 8.0
            , hint "_  _  _  _"
            , hintColor Color.black600
            , id $ EHC.getNewIDWithTag "OTP1"
            , pattern "[0-9]*,4"
            , stroke $ getStroke state
            -- , inputType if DS.length state.props.otp.two == 0 then TypeText else Disabled 
            ] <> FontStyle.title7 CT.TypoGraphy 
        ]
    ]  

    where
        getStroke :: forall w. ST.MeterRideScreenState -> String
        getStroke state = 
            if state.props.invalidOTP then "1," <> Color.red900
            else if state.props.isFocussed then "1," <> Color.blue900
            else "0," <> Color.blue900

invalidOTPView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
invalidOTPView push state =
    linearLayout
    [
        width MATCH_PARENT
        , height $ V 40
        , gravity CENTER
        , margin $ MarginTop 12
    ]
    [
        textView $ [
        height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , text "Invalid OTP. Please check\nagain and re-enter"
        , color Color.red900
        ] <> FontStyle.body5 CT.TypoGraphy
    ]

enterButtonView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
enterButtonView push state =
    linearLayout
    [
    width MATCH_PARENT
    , height WRAP_CONTENT
    , weight 1.0
    , gravity CENTER
    , layoutGravity "center_vertical"
    , orientation VERTICAL
    ]
    [
        linearLayout 
        ([
        width $ V 216
        , height $ V 216
        , cornerRadius 999.0
        , background Color.lightHulkGreen
        , gravity CENTER
        , clickable $ DS.length state.props.otp.one == 4 && state.props.isOTPLoading == false && state.props.invalidOTP == false
        , onClick push $ const EnterOTP
        ] <> if (DS.length state.props.otp.one == 4 && state.props.isOTPLoading == false && state.props.invalidOTP == false) then [rippleColor Color.rippleShade] else [])
        [
            if state.props.isOTPLoading then loaderView push state else enterTextView push state
        ]
    ]

enterTextView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
enterTextView push state = 
    textView $ [
    height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER
    , text "ENTER"
    , letterSpacing $ PX 2.0
    , color Color.white900
    ] <> FontStyle.title3 CT.TypoGraphy 

loaderView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
loaderView push state =
    linearLayout
    [ width $ V 216
    , height $ V 216
    , gravity CENTER
    ]
    [ progressBar
        [ width $ V 109
        , height $ V 108
        , progressBarColor Color.white900
        ]
    ]

infoView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
infoView push state = 
    linearLayout
    [
        background Color.blackLessTrans
        , width MATCH_PARENT
        , height MATCH_PARENT
        , onClick push $ const BackPressed
        , gravity CENTER
        , padding $ Padding 16 16 16 16
    ]
    [
            
        linearLayout
        [
            width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
        ]
        [
            linearLayout
            [
                width MATCH_PARENT
                , height WRAP_CONTENT
                , padding $ Padding 16 16 16 16 
                , background Color.mildYellow
                , cornerRadii $ Corners 15.0 true true false false
            ]
            [
                textView $ [
                height WRAP_CONTENT
                , weight 1.0
                , text "Namma Meter Rides"
                , color Color.black900
                ] <> FontStyle.h2 CT.TypoGraphy
                , imageView [
                imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_meter_icon_yellow"
                , height $ V 40
                , width $ V 40
                ]
            ]
            , rateCardContent push state
        ]
    ]

rateCardContent :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
rateCardContent push state = 
    linearLayout
    [
        width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 16 16 16 16 
        , background Color.white900
        , orientation VERTICAL
        , cornerRadii $ Corners 15.0 false false true true
    ]
    [
        textView $ [
        height WRAP_CONTENT
        , weight 1.0
        , gravity CENTER
        , text "Take rides at meter rates!"
        , color Color.black900
        ] <> FontStyle.subHeading1
         CT.TypoGraphy
        , imageView [
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_whats_meter_ride"
        , height $ V 200
        , width MATCH_PARENT
        , margin $ Margin 0 16 0 16
        ]
        , bulletView push state true "Now take meter rides with any Namma"
        , bulletView push state false "Yatri enabled driver easily"
        , bulletView push state true "Track your fares in real time and enter"
        , bulletView push state false "your destination for a seamless journey"
        , bulletView push state true "To start, just ask your driver for a rideOTP"
        , linearLayout
        [
            width MATCH_PARENT
            , height WRAP_CONTENT
            , padding $ Padding 16 16 16 16 
            , background Color.black900
            , cornerRadius 8.0 
            , gravity CENTER
            , onClick push $ const BackPressed
            , margin $ MarginTop 28
        ]
        [
            textView $ [
            height WRAP_CONTENT
            , weight 1.0
            , gravity CENTER
            , text "Got It"
            , color Color.yellow900
            ] <> FontStyle.subHeading3 CT.TypoGraphy
        ]
    ]

bulletView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> Boolean -> String -> PrestoDOM (Effect Unit) w
bulletView push state isBullet txtValue = 
    linearLayout
    [
        width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 5 5 5 0
        , gravity CENTER

    ]
    [
        linearLayout
        [
            width $ V 5
            , height $ V 5
            , cornerRadius 100.0
            , background Color.black700
            , alpha if isBullet then 1.0 else 0.0
            , margin $ Margin 0 3 9 0
        ]
        [

        ]
        , textView $ [
        height WRAP_CONTENT
        , weight 1.0
        , text txtValue
        , color Color.black700
        ] <> FontStyle.subHeading2 CT.TypoGraphy
    ]