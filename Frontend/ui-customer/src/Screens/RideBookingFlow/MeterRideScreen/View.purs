module Screens.RideBookingFlow.MeterRideScreen.View where

import Prelude
import Screens.Types as ST
import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import PrestoDOM.Animation as PrestoAnim
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (<<<), (<>), (==), (>), not, void, discard, (-), show, (*), (<=), (>=), (/))
import PrestoDOM (Gravity(..), FontWeight(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, shimmerFrameLayout, relativeLayout, gravity, height, alpha, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, singleLine, id, frameLayout, scrollBarY, fillViewport, onAnimationEnd, rippleColor, alignParentBottom, progressBar, letterSpacing, editText, hint, hintColor, pattern, clickable, adjustViewWithKeyboard, onChange, onFocus, inputType, progressBarColor, maxLines, fontWeight, ellipsize)
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
import Data.Array as DA
import Font.Size as FontSize
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
    Anim.screenAnimation $ linearLayout
    [
        width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.white900
        , adjustViewWithKeyboard "true"
    ]
    [
        if state.data.isMeterRideSynced then meterRideSyncedView push state else meterRideInitView push state
    ]

meterRideInitView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
meterRideInitView push state = 
    relativeLayout
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
        , onClick push $ const ShowInfoCard
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
        , clickable $ DS.length state.props.otp == 4 && state.props.isOTPLoading == false && state.props.invalidOTP == false
        , onClick push $ const EnterOTP
        ] <> if (DS.length state.props.otp == 4 && state.props.isOTPLoading == false && state.props.invalidOTP == false) then [rippleColor Color.rippleShade] else [])
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


meterRideSyncedView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
meterRideSyncedView push state = 
    relativeLayout
    [
        width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
    ]
    [
        linearLayout
        [
            width MATCH_PARENT
            , height MATCH_PARENT
            , background Color.white900
            , orientation VERTICAL
        ]
        [
            syncedTopBarView push state
            , syncedMainView push state
        ]
        , rateCardView push state
    ]

syncedMainView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
syncedMainView push state = 
    linearLayout
    [
        width MATCH_PARENT
        , weight 1.0
        , padding $ Padding 26 32 26 20
        , orientation VERTICAL
    ]
    [
        fareView push state
        , distAndTimeView push state
        , linearLayout
        [ gravity BOTTOM
        , weight 1.0
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ 
        enterDestinationView push state
        ]
    ]

syncedTopBarView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
syncedTopBarView push state = 
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
            , gravity CENTER
        ]
        [
            textView $ [
                height WRAP_CONTENT
                , weight 1.0
                , text $ getString METER_RIDE
                , color Color.black900
            ] <> FontStyle.h3 CT.TypoGraphy
            , linearLayout
            [
                width WRAP_CONTENT
                , height WRAP_CONTENT
                , cornerRadius 20.0
                , stroke $ "1," <> Color.grey600
                , padding $ Padding 14 8 14 8
                , margin $ MarginRight 8
            ]
            [
                imageView [
                imageWithFallback $ fetchImage FF_ASSET "ny_ic_forward_arrow"
                , height $ V 20
                , width $ V 20
                ]
            ]
            , linearLayout
            [  
                width WRAP_CONTENT
                , height WRAP_CONTENT
                , padding $ Padding 12 8 12 8
                , cornerRadius 20.0
                , stroke $ "1," <> Color.grey600
            ]
            [
                imageView [
                imageWithFallback $ fetchImage FF_ASSET "ny_ic_shield_orange"
                , height $ V 20
                , width $ V 20
                , margin $ MarginRight 6
                ]
                , textView $ [
                height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getString METER_RIDE
                , color Color.black900
            ] <> FontStyle.body6 CT.TypoGraphy
            ]
        ]
        , linearLayout
        [
            width MATCH_PARENT
            , height $ V 1
            , background $ Color.grey600
        ]
        [

        ]
    ]

fareView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
fareView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ Margin 0 32 0 0
    , cornerRadius 32.0
    , orientation VERTICAL
    , background Color.blue600
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , padding $ Padding 0 35 0 12
        , onClick push $ const ShowRateCard
        ]
        [ textView
            $ [ text $ getString FARE
              , height WRAP_CONTENT
              , color Color.black600
              , letterSpacing $ PX 1.0
              , margin $ Margin 0 0 8 0
              , padding $ PaddingBottom 3
              ]
            <> FontStyle.body30 CT.TypoGraphy
        , imageView
            [ height $ V 20
            , width $ V 20
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_blue_inverse"
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , padding $ Padding 0 13 0 44
        ]
        [ linearLayout
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , padding $ Padding 0 2 14 0
            ]
            [ textView
                $ [ text "₹"
                  , color Color.black600
                  , gravity TOP_VERTICAL
                  ]
                <> FontStyle.body36 CT.TypoGraphy
            ]
        , sevenSegmentView push state
        , textView
            $ [ text $ getString UPTON2KM
              , color Color.black900
              , margin $ MarginLeft 5
              , height MATCH_PARENT
              , gravity BOTTOM
              , alpha $ if state.data.distance < 2.0 then 1.0 else 0.4
              ]
            <> (FontStyle.body6 CT.TypoGraphy)
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , padding $ PaddingBottom 16
        ]
        [ linearLayout
            [ height $ V 12
            , width $ V 12
            , background Color.black
            , cornerRadius 6.0
            ]
            [ PrestoAnim.animationSet
                [ PrestoAnim.Animation
                    [ PrestoAnim.duration 600
                    , PrestoAnim.fromAlpha 0.0
                    , PrestoAnim.toAlpha 1.0
                    , PrestoAnim.repeatMode PrestoAnim.Reverse
                    , PrestoAnim.repeatCount PrestoAnim.Infinite
                    , PrestoAnim.interpolator PrestoAnim.EaseOut
                    ]
                    true
                ]
                $ linearLayout
                    [ height $ V 12
                    , width $ V 12
                    , background "#E91212"
                    , cornerRadius 6.0
                    ]
                    []
            ]
        , textView
            $ [ text $ getString UPDATED_AT
              , color Color.black600
              , margin $ MarginLeft 8
              , letterSpacing $ PX 2.0
              , padding $ PaddingBottom 3
              ]
            <> FontStyle.body22 CT.TypoGraphy

            , linearLayout
                [ height $ WRAP_CONTENT
                , width $ WRAP_CONTENT
                , gravity CENTER
                , onClick push $ const RefreshTime
                , rippleColor Color.rippleShade
                ]
                [ textView
                    $ [ text $ "  " <> EHC.convertUTCtoISC state.data.lastUpdatedTime "h:mm A"
                      , color Color.black900
                      , padding $ PaddingBottom 3
                      ]
                    <> FontStyle.body22 CT.TypoGraphy
                , PrestoAnim.animationSet
                    [ PrestoAnim.Animation
                        [ PrestoAnim.duration 500
                        , PrestoAnim.fromRotation 0
                        , PrestoAnim.toRotation 360
                        , PrestoAnim.repeatCount PrestoAnim.NoRepeat
                        ]
                        state.props.refreshAnimation
                    ]
                    $ imageView
                        [ width $ V 16
                        , height $ V 16
                        , onAnimationEnd push StopRotation
                        , margin $ MarginLeft 8
                        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_refresh"
                        ]
                ]
        ]
    ]

sevenSegmentView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
sevenSegmentView push state =
  let digits = getDigits state.props.meterFare  
      len = DA.length digits
      finalDigits = if state.props.meterFare == 0 then [-1,-1,-1,-1] else if len > 4 then digits else getArray (4 - len) <> digits
  in
    linearLayout
      [ width WRAP_CONTENT
      , height MATCH_PARENT
      ](map (\item -> textView
          $ [ text $ if item == -1 then "0" else show item 
            , textSize FontSize.a_48
            , fontWeight $ FontWeight 400
            , color if item == -1 then Color.black600 else Color.black900
            , fontStyle "https://assets.moving.tech/beckn/fonts/DigitalNumbers-Regular.ttf"
            , letterSpacing $ PX 1.0
            ]) finalDigits)
      

getDigits :: Int -> Array Int
getDigits input = if input > 9 then getDigits (div input 10) <> [mod input 10] else [input]

getArray :: Int -> Array Int
getArray count = if count <= 0 then [] else [-1] <> (getArray (count - 1))

rateCardView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    , gravity CENTER
    , onClick push $ const CloseRateCard
    , visibility $ boolToVisibility state.props.showRateCard
    ]
    [ PrestoAnim.animationSet
        [ PrestoAnim.Animation
            [ PrestoAnim.duration 100
            , PrestoAnim.fromScaleY 0.0
            , PrestoAnim.toScaleY 1.0
            , PrestoAnim.fromScaleX 0.0
            , PrestoAnim.toScaleX 1.0
            ]
            state.props.showRateCard
        ]
        $ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , cornerRadius 15.0
            , margin $ Margin 16 0 16 0
            , padding $ Padding 10 10 10 10
            , orientation VERTICAL
            , onClick push $ const NoAction
            , background Color.blue600
            ]
            [ textView
                $ [ text $ getString RATE_CARD
                  , height WRAP_CONTENT
                  , width MATCH_PARENT
                  , color Color.black800
                  , gravity CENTER
                  ]
                <> FontStyle.h2 CT.TypoGraphy
            , linearLayout
                [ margin $ Margin 16 16 16 0
                , orientation VERTICAL
                , background Color.white900
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER
                , cornerRadius 12.0
                ]
                [ textView
                    $ [ text $ getString AUTO_RICKSHAW
                      , height WRAP_CONTENT
                      , width MATCH_PARENT
                      , color Color.black800
                      , gravity CENTER
                      , margin $ Margin 10 10 10 6
                      ]
                    <> FontStyle.subHeading1 CT.TypoGraphy
                , imageView
                    $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_auto_right_view"
                      , width $ V 81
                      , height $ V 81
                      , margin $ Margin 10 6 10 0
                      , gravity CENTER
                      ]
                , relativeLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    ]
                    [ PrestoAnim.animationSet
                        [ Anim.fadeIn $ state.props.isRateCardLoading
                        , Anim.fadeOut $ not state.props.isRateCardLoading
                        ]
                        $ linearLayout
                            [ height WRAP_CONTENT
                            , width MATCH_PARENT
                            , orientation VERTICAL
                            , gravity CENTER
                            ]
                            [ shimmerFrameLayout
                                [ height WRAP_CONTENT
                                , width WRAP_CONTENT
                                , cornerRadius 8.0
                                , background Color.grey900
                                , margin $ Margin 10 0 10 2
                                ]
                                [ textView
                                    $ [ height WRAP_CONTENT
                                      , width MATCH_PARENT
                                      , text $ "₹" <> show (state.props.rateCardConfig.sliderFare)
                                      , visibility INVISIBLE
                                      ]
                                    <> FontStyle.priceFont CT.TypoGraphy
                                ]
                            , shimmerFrameLayout
                                [ height WRAP_CONTENT
                                , width WRAP_CONTENT
                                , cornerRadius 4.0
                                , background Color.grey900
                                , margin $ Margin 10 3 10 20
                                ]
                                [ textView
                                    $ [ height WRAP_CONTENT
                                      , width MATCH_PARENT
                                      , text $ "₹" <> show state.props.rateCardConfig.ratePerKM <> "/km"
                                      , visibility INVISIBLE
                                      ]
                                    <> FontStyle.body3 CT.TypoGraphy
                                ]
                            ]
                    , PrestoAnim.animationSet
                        [ Anim.fadeIn $ not state.props.isRateCardLoading
                        , Anim.fadeOut state.props.isRateCardLoading
                        ]
                        $ linearLayout
                            [ height WRAP_CONTENT
                            , width MATCH_PARENT
                            , orientation VERTICAL
                            ]
                            [ textView
                                $ [ text $ "₹" <> show (state.props.rateCardConfig.sliderFare)
                                  , height WRAP_CONTENT
                                  , width MATCH_PARENT
                                  , color Color.black800
                                  , margin $ Margin 10 0 10 2
                                  , gravity CENTER
                                  ]
                                <> FontStyle.priceFont CT.TypoGraphy
                            , textView
                                $ [ text $ "₹" <> show state.props.rateCardConfig.ratePerKM <> "/km"
                                  , height WRAP_CONTENT
                                  , width MATCH_PARENT
                                  , color Color.black500
                                  , margin $ Margin 10 0 10 20
                                  , gravity CENTER
                                  ]
                                <> FontStyle.body23 CT.TypoGraphy
                            ]
                    ]
                ]
            , sliderView push state
            , textView
                $ [ text $ getString GOT_IT
                  , height WRAP_CONTENT
                  , width MATCH_PARENT
                  , color Color.blue800
                  , gravity CENTER
                  , margin $ Margin 16 16 16 16
                  , onClick push $ const CloseRateCard
                  ]
                <> FontStyle.h2 CT.TypoGraphy
            ]
    ]

distAndTimeView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
distAndTimeView push state =
  linearLayout
    [ width MATCH_PARENT
    , height $ WRAP_CONTENT
    , margin $ Margin 20 36 20 0
    , background $ Color.white900
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , orientation VERTICAL
        , weight 1.0
        ]
        [ linearLayout
            [ gravity CENTER
            ]
            [ imageView
                [ height $ V 25
                , width $ V 25
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_ny_location_arrow_in"
                , margin $ Margin 0 0 10 0
                ]
            , textView
                $ [ text $ getString DIST
                  , height WRAP_CONTENT
                  , letterSpacing $ PX 2.0
                  , color Color.black500
                  ]
                <> FontStyle.h2 CT.TypoGraphy
            ]
        , linearLayout
            [ gravity BOTTOM
            , margin $ Margin 0 5 0 0
            ]
            [ textView
                $ [ text $ show state.data.distance
                  , height WRAP_CONTENT
                  , color Color.black900
                  ]
                <> FontStyle.body34 CT.TypoGraphy
            , textView
                $ [ text "KM"
                  , height WRAP_CONTENT
                  , letterSpacing $ PX 2.0
                  , margin $ Margin 4 0 0 0
                  , color Color.black500
                  ]
                <> FontStyle.h2 CT.TypoGraphy
            ]
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity RIGHT
        , orientation VERTICAL
        ]
        [ linearLayout
            []
            [ imageView
                [ height $ V 19
                , width $ V 19
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_ny_clock_unfilled"
                , margin $ Margin 0 4 10 0
                ]
            , textView
                $ [ text $ getString TIME
                  , height WRAP_CONTENT
                  , margin $ Margin 0 0 5 0
                  , letterSpacing $ PX 2.0
                  , color Color.black500
                  ]
                <> FontStyle.h2 CT.TypoGraphy
            ]
        , linearLayout
            [ gravity CENTER
            , margin $ Margin 0 5 0 0
            ]
            [ textView
                $ [ text $ getTime state.data.timeSec
                  , height WRAP_CONTENT
                  , color Color.black900
                  ]
                <> FontStyle.body34 CT.TypoGraphy
            ]
        ]
    ]
  where
  getTime :: Int -> String
  getTime sec =
    if sec <= 0 then
      "--:--"
    else
      let
        sstr = if (div (mod sec 60) 10) == 0 then "0" <> (show (mod sec 60)) else show (mod sec 60)

        mstr = if (div (mod (div sec 60) 60) 10) == 0 then "0" <> (show (mod (div sec 60) 60)) else show (mod (div sec 60) 60)

        hrs = div sec 3600
      in
        if hrs == 0 then
          mstr <> ":" <> sstr
        else
          (show hrs) <> ":" <> mstr <> ":" <> sstr

enterDestinationView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
enterDestinationView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.blue600
    , gravity CENTER
    , padding $ Padding 15 15 15 15
    , margin $ Margin 0 0 0 20
    , cornerRadius 15.0
    , onClick push (const $ EnterDestination)
    , rippleColor Color.rippleShade
    , clickable  (state.data.destinationAddress == "")
    ]
    [ imageView
        [ height $ V 20
        , width $ V 20
        , color Color.blue800
        , margin $ Margin 0 2 10 0
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_location_blue"
        , visibility $ boolToVisibility (state.data.destinationAddress == "")
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , visibility $ boolToVisibility (state.data.destinationAddress /= "")
        , orientation VERTICAL
        , width MATCH_PARENT
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 5 5 5 5
            ]
            [ textView
                $ [ text $ getString DESTINATION
                  , color "#A7A7A7"
                  , onClick
                      ( \action -> do
                          _ <- push action
                          pure unit
                      )
                      (const $ EnterDestination)
                  , clickable (state.data.destinationAddress == "")
                  ]
                <> FontStyle.body5 CT.TypoGraphy
            ]
        , textView
            $ [ text state.data.destinationAddress
              , maxLines 2
              , ellipsize true
              , padding $ Padding 5 5 5 5
              , color Color.black800
              , clickable  (state.data.destinationAddress == "")
              , onClick
                  ( \action -> do
                      _ <- push action
                      pure unit
                  )
                  (const $ EnterDestination)
              ]
            <> FontStyle.body5 CT.TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 10 10 10 10
            , gravity RIGHT
            , clickable true
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding $ Padding 10 10 10 10
                , gravity CENTER
                , cornerRadius 22.0
                , background Color.blue900
                , rippleColor Color.rippleShade
                , onClick
                    ( \action -> do
                        _ <- push action
                        pure unit
                    )
                    (const $ OnNavigate)
                ]
                [ imageView
                    [ height $ V 20
                    , width $ V 20
                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_google_map_tracking"
                    ]
                , textView
                    $ [ text $ getString MAPS
                      , height WRAP_CONTENT
                      , color Color.white900
                      , padding $ PaddingBottom 3
                      ]
                    <> FontStyle.body23 CT.TypoGraphy
                ]
            ]
        ]
    , textView
        $ [ text "Enter Destination" -- $ getString ENTER_DESTINATION
          , height WRAP_CONTENT
          , color Color.blue800
          , visibility $ boolToVisibility (state.data.destinationAddress == "")
          ]
        <> FontStyle.h3 CT.TypoGraphy
    ]

sliderView :: forall w. (Action -> Effect Unit) -> ST.MeterRideScreenState -> PrestoDOM (Effect Unit) w
sliderView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , cornerRadius 12.0
    , background Color.white900
    , gravity CENTER
    , padding $ Padding 16 16 16 16
    , margin $ Margin 16 16 16 0
    ]
    [ textView
        $ [ text "Choose Ride Distance" -- $ getString CHOOSE_RIDE_DIST
          , color Color.black700
          ]
        <> FontStyle.body1 CT.TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , margin $ Margin 5 5 5 5
        ]
        [ imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_min_blue"
              , width $ V 40
              , height $ V 40
              , stroke $ "1," <> Color.grey900
              , onClick push $ const $ ChangeSlider false
              , cornerRadius 24.0
              , alpha decButtonAlpha
              , clickable decButtonEnabled
              , gravity CENTER
              , margin $ Margin 0 5 15 5
              ]
            <> if decButtonEnabled then [ rippleColor Color.rippleShade ] else []
        , textView
            $ [ text $ show state.props.rateCardConfig.sliderVal <> " km"
              , color Color.black800
              , gravity CENTER
              , margin $ Margin 0 0 0 5
              ]
            <> FontStyle.priceFont CT.TypoGraphy
        , imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_plus_blue"
              , width $ V 40
              , height $ V 40
              , stroke $ "1," <> Color.grey900
              , onClick push $ const $ ChangeSlider true
              , cornerRadius 24.0
              , alpha incButtonAlpha
              , clickable incButtonEnabled
              , margin $ Margin 15 5 0 5
              , gravity CENTER
              ]
            <> if incButtonEnabled then [ rippleColor Color.rippleShade ] else []
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 5 5 5 5
        , id $ EHC.getNewIDWithTag "RateSliderTool"
        ]
        []
    , PrestoAnim.animationSet [ Anim.triggerOnAnimationEnd true ]
        $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , id $ EHC.getNewIDWithTag "RateSlider"
            , margin $ Margin 5 5 5 5
            , onAnimationEnd
                ( \action ->
                    void
                      $ JB.renderSlider
                          ( \sliderAction -> do
                              void $ pure $ JB.updateInputString "MeterRideInput"
                              push sliderAction
                              void $ JB.debounceFunction 800 push DebounceCallBack false
                              pure unit
                          )
                          SliderCallback
                          sliderConfig
                )
                (const NoAction)
            ]
            []
    , textView
        $ [ text "Rate changes as the distance changes" -- $ getString RATE_CHANGES_AS_THE_DISTANCE_CHANGES
          , color Color.black700
          , margin $ Margin 10 10 10 5
          ]
        <> FontStyle.body3 CT.TypoGraphy
    ]
  where
  incButtonAlpha = if incButtonEnabled then 1.0 else 0.4

  decButtonAlpha = if decButtonEnabled then 1.0 else 0.4

  incButtonEnabled = state.props.rateCardConfig.sliderVal < state.props.rateCardConfig.sliderMaxValue

  decButtonEnabled = state.props.rateCardConfig.sliderVal > state.props.rateCardConfig.sliderMinValue

  sliderConfig =
    JB.sliderConfig
      { id = EHC.getNewIDWithTag "RateSlider"
      , toolTipId = EHC.getNewIDWithTag "RateSliderTool"
      , sliderMinValue = state.props.rateCardConfig.sliderMinValue
      , sliderMaxValue = state.props.rateCardConfig.sliderMaxValue
      , sliderDefaultValue = state.props.rateCardConfig.sliderDefVal
      , stepFunctionForCoinConversion = state.props.rateCardConfig.incrementUnit
      , enableToolTip = false
      , getCallbackOnProgressChanged = true
      , thumbColor = Color.blue800
      , bgColor = Color.grey900
      , progressColor = Color.blue800
      , bgAlpha = 1000
      }
