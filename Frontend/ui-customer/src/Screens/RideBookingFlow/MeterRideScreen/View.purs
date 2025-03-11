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
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, relativeLayout, gravity, height, alpha, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, singleLine, id, frameLayout, scrollBarY, fillViewport, onAnimationEnd, rippleColor, alignParentBottom, progressBar, letterSpacing)
import Screens.RideBookingFlow.MeterRideScreen.Controller (Action(..), eval, ScreenOutput)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import PrestoDOM.Types.Core (LetterSpacing(..))
import Mobility.Prelude (boolToVisibility)
import Screens.Types as ST
import Styles.Colors as Color
import JBridge as JB
import Debug
import Common.Types.App as CT
import Font.Style as FontStyle

screen :: ST.MeterRideScreenState -> Screen Action ST.MeterRideScreenState ScreenOutput
screen initialState =
    { initialState
    , view: view
    , name: "MeterRideScreen"
    , globalEvents: []
    , eval:
        ( \state action -> do
            let _ = spy "PickupInstructionsScreen state" state
            let _ = spy "PickupInstructionsScreen action" action
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
    ] $ 
    [
        meterRideView push state
    ]

meterRideView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
meterRideView push state = 
    linearLayout
    [
        width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
    ]
    [
        topBarView push state
        , imageView [
        imageWithFallback $ fetchImage FF_ASSET "ny_ic_meterRide_loading"
        , height $ V 120
        , width MATCH_PARENT

        ]
        , linearLayout[
            gravity CENTER
            , width MATCH_PARENT
            , height WRAP_CONTENT
        ]
        [
            textView $ [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , text "Enter Ride OTP"
            , color Color.black600
            , letterSpacing $ PX 2.0
            ] <> FontStyle.h3 CT.TypoGraphy
        ]
        , otpView push state
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
    ]
    [
        singleNumberOTPView push state
        , singleNumberOTPView push state
        , singleNumberOTPView push state
        , singleNumberOTPView push state
    ]  

singleNumberOTPView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
singleNumberOTPView push state =
    linearLayout [
        width WRAP_CONTENT
        , height WRAP_CONTENT
        , padding $ Padding 18 9 18 9
    ]
    [
        editText $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , color Color.black900
        --   , onChange push $ ReferralTextChanged
        --   , onFocus push $ ReferralTextFocusChanged
          , gravity CENTER
          , cornerRadius 8.0
          , hint "_"
          , hintColor Color.black600
          , pattern "[0-9.]*,1"
          , id $ EHC.getNewIDWithTag "CustomerMeterRideOTPText"
          ] <> FontStyle.title7 LanguageStyle 
    ]

invalidOTPView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
invalidOTPView push state =
    linearLayout
    [
        width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
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