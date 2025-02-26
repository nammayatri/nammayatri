{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DriverInfoCard.View where

import Common.Types.App
import Components.DriverInfoCard.Common.Types
import Components.DriverInfoCard.Common.View
import Locale.Utils
import CarouselHolder as CarouselHolder
import Common.Styles.Colors as CommonColor
import Animation (fadeIn, fadeInWithDelay, scaleYAnimWithDelay, shimmerAnimation, translateInXAnim)
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..), City(..))
import Components.BannerCarousel as BannerCarousel
import Components.DriverInfoCard.Controller (Action(..), DriverInfoCardState)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Function.Uncurried (runFn1)
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String as STR
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String (Pattern(..), split, length, take, drop, replaceAll, Replacement(..), contains, toLower, null)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Suggestions (getMessageFromKey, chatSuggestion)
import Engineering.Helpers.Utils (showAndHideLoader)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, safeMarginBottom, screenWidth, getCurrentUTC, getUTCAfterNSeconds, convertUTCtoISC, getUTCAfterNSecondsImpl)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), userCommonAssetBaseUrl, getPaymentMethod, secondsToHms, makeNumber, getVariantRideType, getTitleConfig, getCityNameFromCode, getDefaultPixelSize, getDistanceBwCordinates,disableChat)
import Helpers.SpecialZoneAndHotSpots (specialZoneTagConfig)
import Helpers.Utils (parseFloat)
import JBridge (fromMetersToKm, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types (DriverInfoConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility, capitalize)
import PrestoDOM (BottomSheetState(..), Accessiblity(..), Gradient(..), Shadow(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, alignParentLeft, alignParentRight, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, shimmerFrameLayout, rippleColor, clipChildren, shadow, clipToPadding, rotation, horizontalScrollView, disableKeyboardAvoidance, scrollBarX, layoutGravity, nestedScrollView, lottieAnimationView)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, negate, (>), (<), (-), (*), bind, pure, discard, not, (&&), (||), (/=),(+), (+), void)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.List as PrestoList
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.LocalizableV2.Strings (getEN)
import Screens.Types (Stage(..), ZoneType(..), SheetState(..), NavigationMode(..))
import Storage (KeyStore(..))
import Storage ( getValueToLocalStore, KeyStore(..)) as STO
import Styles.Colors as Color
import Common.Styles.Colors as CommonColor
import Engineering.Helpers.Utils (showAndHideLoader)
import Types.App (defaultGlobalState)
import Helpers.Utils as HU
import Engineering.Helpers.Suggestions (getMessageFromKey, chatSuggestion)
import JBridge(fromMetersToKm, getLayoutBounds, differenceBetweenTwoUTC)
import Data.Int(toNumber, fromString)
import MerchantConfig.Types (DriverInfoConfig)
import Mobility.Prelude (boolToVisibility, capitalize)
import Locale.Utils
import Components.DriverInfoCard.Common.View
import Components.DriverInfoCard.Common.Types
import Components.DriverInfoCard.Controller as Controller
import Data.Function.Uncurried (runFn1)
import CarouselHolder as CarouselHolder
import Components.BannerCarousel as BannerCarousel
import PrestoDOM.List as PrestoList
import Timers (rideDurationTimer)
import Data.Function.Uncurried (runFn2)
import Data.Int (floor, toNumber)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (FareProductType(..)) as FPT
import Data.String as STR
import JBridge as JB
import Animation as Anim
import Screens.Types as ST

view :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  let enableShareRide = state.data.config.feature.enableShareRide && isStageAfterRideAccepted
      enableSupport = state.data.config.feature.enableSupport && isStageAfterRideAccepted
      isStageAfterRideAccepted = (Array.any (_ == state.props.currentStage) ) [RideAccepted, RideStarted, ChatWithDriver] 
  in
  PrestoAnim.animationSet
          [ scaleYAnimWithDelay 10] $ linearLayout
  [ height WRAP_CONTENT
  , width $ V (screenWidth unit)
  , orientation VERTICAL
  , id $ getNewIDWithTag "BottomSheetLayout"
  , onAnimationEnd push $ const $ NoAction
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , accessibility ENABLE
          , id $ getNewIDWithTag "DriverInfoCardActionView"
          , clickable true
          ][ otpAndWaitView push state 
            , endOTPView push state
            , deliveryImageAndOtpView push state
            , linearLayout [
                weight 1.0
              , visibility $ boolToVisibility $ state.props.currentStage == RideStarted
            ] []
            -- , if state.props.currentStage == RideStarted || state.props.stageBeforeChatScreen == RideStarted then trackRideView push state else dummyView push -- TODO: may use in future: Ashish Singh
            , if enableShareRide then shareRideButton push state else if enableSupport then contactSupport push state else dummyView push -- TEMP FIX UNTIL THE NEW DESIGN IS DONE
            ]
    ]
  , driverInfoViewSpecialZone push state
  , driverInfoView push state
  ]


endOTPView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
endOTPView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , cornerRadius if os == "IOS" then 18.0 else 32.0
  , background Color.white900
  , gravity CENTER
  , clickable true
  , accessibility DISABLE
  , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
  , visibility $ boolToVisibility $ Array.any (_ == state.data.fareProductType) [FPT.RENTAL, FPT.INTER_CITY] && (state.props.currentStage == RideStarted || state.props.stageBeforeChatScreen == RideStarted)
  , margin $ Margin 8 8 6 8
  ]
  [ textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , accessibilityHint $ " END O T P : " <> (STR.replaceAll (STR.Pattern "") (STR.Replacement " ")  state.data.otp) 
    , accessibility ENABLE
    , text $ getString END_OTP
    , padding $ Padding 12 0 4 if os == "IOS" then 0 else 3
    , color Color.black700
    ] <> FontStyle.body22 TypoGraphy
  , linearLayout 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , background Color.grey700
    , clickable true
    , onClick push $ const ShowEndOTP
    , visibility $ boolToVisibility $ not state.props.showEndOTP
    , margin $ Margin 4 4 4 4
    , padding $ Padding 8 4 8 4
    , cornerRadius 16.0
    , rippleColor Color.rippleShade
    ]
    [ imageView 
      [ gravity CENTER_VERTICAL
      , height $ V 22
      , width $ V 22
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
      ] 
    ]
  , linearLayout
    [ height $ WRAP_CONTENT
    , width $ WRAP_CONTENT
    , cornerRadius 16.0
    , visibility $ boolToVisibility state.props.showEndOTP
    ]
    [ PrestoAnim.animationSet [translateInXAnim $ endOTPAnimConfig state]
      $ textView $ 
      [ text $ state.data.otp 
      , color Color.black900
      , cornerRadius if os == "IOS" then 12.0 else 16.0
      , padding $ Padding 8 4 8 6
      , margin $ Margin 0 4 4 4
      , background Color.grey700
      ] <> FontStyle.body22 TypoGraphy
    ]
  ]
deliveryImageAndOtpView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
deliveryImageAndOtpView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , cornerRadius if os == "IOS" then 18.0 else 32.0
  , background Color.white900
  , gravity CENTER
  , clickable true
  , accessibility DISABLE
  , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
  , visibility $ boolToVisibility $ state.data.fareProductType == FPT.DELIVERY
  , margin $ Margin 8 8 6 8
  ][ textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , accessibilityHint $ " O T P : " <> (STR.replaceAll (STR.Pattern "") (STR.Replacement " ")  state.data.otp) 
    , accessibility ENABLE
    , text $ getString PACKAGE_PHOTO_AND_OTP
    , padding $ Padding 12 0 4 if os == "IOS" then 0 else 3
    , color Color.black700
    ] <> FontStyle.body22 TypoGraphy
  , linearLayout 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , background Color.grey700
    , clickable true
    , onClick push $ const ShowDeliveryImageAndOtp
    , margin $ Margin 4 4 4 4
    , padding $ Padding 8 4 8 4
    , cornerRadius 16.0
    , rippleColor Color.rippleShade
    ]
    [ imageView 
      [ gravity CENTER_VERTICAL
      , height $ V 22
      , width $ V 22
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
      ] 
    ]
  ]

driverInfoViewSpecialZone :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoViewSpecialZone push state =
  let 
    currentCityConfig = HU.getCityConfig  state.data.config.cityConfig (show state.props.merchantCity)
    brandingBannerViewVis = if currentCityConfig.iopConfig.enable then INVISIBLE else GONE
  in 
  linearLayout
  [ width  MATCH_PARENT
  , height WRAP_CONTENT
  , visibility $ boolToVisibility $ isOtpRideFlow state
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.grey700
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true false false
          , stroke $ "1," <> Color.grey900
          ][linearLayout
            [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , orientation VERTICAL
            , id $ getNewIDWithTag "driverInfoViewSpecialZone"
            ][ linearLayout
               [ height $ WRAP_CONTENT
               , width $ MATCH_PARENT
               , gravity CENTER
               ][linearLayout
                 [ gravity CENTER
                 , background Color.transparentGrey
                 , height $ V 4
                 , width $ V 34
                 , accessibility ENABLE
                 , accessibilityHint $ "Bottom Sheet : Scrollable element : " <> if state.data.bottomSheetState == EXPANDED then "Scroll down to collapse details" else  "Scroll up to expand for more ride actions"
                 , clickable true
                 , onClick push $ const ToggleBottomSheet
                 , margin (MarginVertical 8 6)
                 , cornerRadius if os == "IOS" then 2.0 else 4.0
                 ][]
               ]
              , titleAndETA push state
              , driverDetailsView push (getDriverDetails state) "SpecialDriverDetailsView" "SpecialNumberPlate"
              , navigateView push state
              , paymentMethodView push state (getString FARE_ESTIMATE) true "SpecialPaymentMethodView"
              , sizedBox (V 12) MATCH_PARENT
            ]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , background Color.grey700
            ][if not state.data.config.showPickUpandDrop then dummyView push else sourceDestinationView push (getTripDetails state)
              , cancelRideLayout push state
              , brandingBannerView state.data.config.driverInfoConfig brandingBannerViewVis Nothing true ""
            ]
          ]
      ]
  ]

sizedBox :: forall w. Length -> Length -> PrestoDOM ( Effect Unit) w
sizedBox height' width' = 
  linearLayout
  [ height height'
  , width width'
  , accessibility DISABLE
  , accessibility DISABLE_DESCENDANT
  ][]

shareRideButton :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
shareRideButton push state = 
  linearLayout
  [ width $ V 56
  , height $ V 56
  , gravity RIGHT
  , orientation VERTICAL
  , clickable $ state.data.providerType == ONUS
  , accessibility DISABLE
  , clipChildren false
  ][ linearLayout
     [ width $ V 40
     , height $ V 40
     , gravity CENTER
     , background Color.white900
     , stroke $ "1,"<> Color.grey900
     , cornerRadius if os == "IOS" then 20.0 else 32.0
     , clickable $ state.data.providerType == ONUS
     , accessibilityHint "Share Ride : Button : Select to share ride details"
     , accessibility ENABLE
     , onClick push $ const ShareRide
     , margin $ Margin 8 8 8 8
     , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
     , rippleColor Color.rippleShade
     ][ imageView
       [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_share_icon"
       , height $ V 16
       , width $ V 16
       , accessibility DISABLE
       , alpha if state.data.providerType == ONUS then 1.0 else 0.5
       ]
     ]
  ]

contactSupport :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
contactSupport push state =
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , background Color.transparent
  , gravity RIGHT
  , orientation VERTICAL
  , clickable true
  , accessibility DISABLE
  , clipChildren false
  ][  linearLayout
      [ width $ V 40
      , height $ V 40
      , gravity CENTER
      , clickable true
      , margin $ Margin 8 8 8 8
      , background Color.white900
      , stroke $ "1,"<> Color.grey900
      , cornerRadius if os == "IOS" then 20.0 else 32.0
      , onClick push $ const RideSupport
      , accessibilityHint "Contact Support : Button"
      , accessibility ENABLE
      , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
      , rippleColor Color.rippleShade
      ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_contact_support"
        , height $ V 16
        , width $ V 16
        , accessibility DISABLE
        ]
      ]
    ]

otpAndWaitView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
otpAndWaitView push state =
  horizontalScrollView
  [ height $ V 56
  , disableKeyboardAvoidance true
  , scrollBarX false
  , scrollBarY false
  , visibility $ boolToVisibility $ (Array.any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver] && (state.props.stageBeforeChatScreen /= RideStarted && state.data.fareProductType /= FPT.DELIVERY ))
  , gravity CENTER
  , accessibility DISABLE
  , weight 1.0
  ][ linearLayout
     [ height$ V 56
     , width WRAP_CONTENT
     , orientation HORIZONTAL
     , clipChildren false
     , clickable true
     , accessibility DISABLE
     ]([linearLayout
       [ width WRAP_CONTENT
       , height $ V 40
       , cornerRadius if os == "IOS" then 20.0 else 32.0
       , background Color.white900
       , gravity CENTER
       , visibility $ boolToVisibility $ rideNotStarted state
       , clickable true
       , accessibility DISABLE
       , margin $ Margin 8 8 6 8
       , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
       ][ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , accessibilityHint $ "O T P : " <> (STR.replaceAll (STR.Pattern "") (STR.Replacement " ")  state.data.otp)
          , accessibility ENABLE
          , text $ getString OTP
          , padding $ Padding 12 0 4 if os == "IOS" then 0 else 3
          , color Color.black700
          ] <> FontStyle.body26 TypoGraphy
        , otpView push state
        ]
      -- , trackRideView push state -- TODO :: may use in future
     ] <> if (isOtpRideFlow state|| state.data.driverArrived) then 
           [(PrestoAnim.animationSet [ fadeIn true ] $ 
           let isQuotes = isOtpRideFlow state
           in
           linearLayout
           [ width WRAP_CONTENT
           , height $ V 40
           , visibility $ boolToVisibility $ rideNotStarted state
           , cornerRadius if os == "IOS" then 20.0 else 32.0
           , background Color.white900
           , clickable true
           , onClick push $ const $ if state.props.isRateCardAvailable || (Array.any (_ == state.data.fareProductType) [ FPT.RENTAL , FPT.AMBULANCE]) then WaitingInfo else NoAction
           , gravity CENTER
           , accessibility DISABLE
           , margin $ Margin 6 8 8 8
           , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
           ][ textView $ 
             [ width WRAP_CONTENT
             , height WRAP_CONTENT
             , accessibility ENABLE
             , accessibilityHint $ (if isQuotes then "O T P Expiry Info" else "Wait time info : ") <> "Button : Select to learn more about " <> if isQuotes then "O T P Expiry Time" else "wait time"   
             , text $ if isQuotes then getString EXPIRES_IN else getString WAIT_TIME
             , color Color.black700
             , padding $ Padding 12 0 4 if os == "IOS" then 0 else 3
             ] <> FontStyle.body26 TypoGraphy
           , imageView
               [ height $ V 12
               , width  $ V 12
               , gravity CENTER_VERTICAL
               , accessibility DISABLE
               , margin $ MarginRight 4
               , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
               , visibility $ boolToVisibility $ state.props.isRateCardAvailable || Array.any (_ == state.data.fareProductType) [ FPT.RENTAL]
               ]
           , waitTimeView push state
           ])]
         else [])
  ]
  

shineAnimation :: forall w . Int -> Int -> PrestoDOM (Effect Unit) w
shineAnimation height' width' =
  linearLayout
  [ height $ MATCH_PARENT
  , width $ WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , cornerRadius 16.0
  , clipChildren true
  , clipToPadding true
  ][ PrestoAnim.animationSet
    [ shimmerAnimation (-100) width' 1500
    ] $ linearLayout
        [ width $ V $ width'
        , height $ MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][ linearLayout
          [ width $ V 10
          , height $ V $ height'
          , background Color.white200
          , rotation 20.0
          , cornerRadius 2.0
          , margin $ MarginHorizontal 10 8
          ][]
         , linearLayout
           [ width $ V 5
           , height $ V $ height'
           , background Color.white200
           , rotation 20.0
           , cornerRadius 2.0
           , margin $ MarginRight 20
           ][]
        ]
  ]

waitTimeView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
waitTimeView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 16.0
  , margin $ Margin 0 4 4 4
  , background $ colorForWaitTime state
  , gravity CENTER_VERTICAL
  , accessibility DISABLE
  ][ textView $ 
     [ height $ WRAP_CONTENT 
     , width $ V $ 60
     , padding $ Padding 0 4 0 6
     , accessibilityHint $ waitTimeHint state
     , accessibility ENABLE 
     , text $ state.data.waitingTime
     , color Color.black900
     , gravity CENTER
     , singleLine true
     ] <> FontStyle.body26 TypoGraphy
  ]

waitTimeHint :: DriverInfoCardState -> String
waitTimeHint state = (if isOtpRideFlow state then "O T P Expires in : " else "Wait Time : ") <> case STR.split (STR.Pattern ":") state.data.waitingTime of
                        [minutes, seconds] -> do 
                          let min = STR.trim $ minutes
                          let sec = STR.trim $ seconds
                          if min /= "00" then min <> " Minutes and " <> sec <> " Seconds" else sec <> " Seconds" 
                        _ -> ""

colorForWaitTime:: DriverInfoCardState -> String
colorForWaitTime state =
  let waitTime = STR.split (STR.Pattern ":") state.data.waitingTime
  in
  case waitTime of
    [minutes, _] -> 
      let mins = fromMaybe 0 (fromString (STR.trim minutes))
          threshold = if isOtpRideFlow state then mins < 5 else mins > 2
      in
      if threshold then Color.carnation100 else Color.grey700 
    _ -> Color.grey700


otpView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
otpView push state =
  let otpDimensions = runFn1 getLayoutBounds $ getNewIDWithTag "OTPView"
      shimmerHeight = (getDefaultPixelSize (otpDimensions.height)) + 3
      shimmerWidth = (getDefaultPixelSize (otpDimensions.width)) - (if os == "IOS" then 3 else 10)
  in
  relativeLayout
  [ height $ MATCH_PARENT
  , width $ WRAP_CONTENT
  , cornerRadius 16.0
  , accessibility DISABLE_DESCENDANT
  ][linearLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , cornerRadius 16.0
      , id $ getNewIDWithTag "OTPView"
      , margin $ Margin 0 4 4 4
      , gravity CENTER_VERTICAL
      , background Color.grey700
      ][ textView $ 
        [ text $ state.data.otp
        , color Color.black900
        , width MATCH_PARENT
        , height MATCH_PARENT
        , padding $ Padding 8 4 8 6
        ] <> FontStyle.body26 TypoGraphy
      ]
  , shineAnimation shimmerHeight shimmerWidth
  ]

trackRideView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
trackRideView push state =
  PrestoAnim.animationSet[fadeIn true] $
  linearLayout
  [ height $ V 56
  , width WRAP_CONTENT
  , clipChildren false
  , clickable true
  , visibility $ boolToVisibility $ (not $ rideNotStarted state) || state.props.stageBeforeChatScreen == RideStarted
  ][ linearLayout
    [ height $ V 40
    , width WRAP_CONTENT
    , background Color.white900
    , cornerRadius if os == "IOS" then 20.0 else 32.0
    , padding $ PaddingHorizontal 12 12
    , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
    , rippleColor Color.rippleShade
    , clickable true
    , margin $ Margin 8 8 8 8
    , onClick push $ const $ StartLocationTracking "GOOGLE_MAP"
    , gravity CENTER
    , accessibility ENABLE
    , accessibilityHint $ "Real Time Tracking on Google Maps : Button"
    , accessibility DISABLE_DESCENDANT
    ][ linearLayout
      [ height $ WRAP_CONTENT
      , width $ WRAP_CONTENT
      ][ imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_nav_grey"
          , height $ V 16
          , width $ V 16
          , accessibilityHint "Track on Gooole Maps : Button"
          , accessibility ENABLE
          , margin $ MarginRight 4
          ]
        , textView $ 
          [ text $ getString TRACK_ON_GOOGLE_MAPS
          , color Color.black800
          ] <> FontStyle.body9 TypoGraphy
      ]
    ]
  ]

titleAndETA :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
titleAndETA push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  ][ if rideNotStarted state then specialZoneHeader state $ STO.getValueToLocalStore STO.SELECTED_VARIANT 
     else distanceView push state
  ]

specialZoneHeader :: forall w. DriverInfoCardState -> String -> PrestoDOM ( Effect Unit) w
specialZoneHeader state vehicleVariant =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 16 16
  , margin $ MarginTop 6
  , accessibility ENABLE
  , accessibilityHint $ "Board the first" <> (fromMaybe (getTitleConfig vehicleVariant).text state.data.serviceTierName) <> (getEN $ TAXI_FROM_ZONE "TAXI_FROM_ZONE")
  , accessibility DISABLE_DESCENDANT
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ][  textView $
          [ text $ getString BOARD_THE_FIRST <> " "
          , color Color.black800
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ (fromMaybe (getTitleConfig vehicleVariant).text state.data.serviceTierName) <> " "
          , color $ (getTitleConfig vehicleVariant).color
          , height WRAP_CONTENT
          , visibility if ((getLanguageLocale languageKey)  == "ML_IN") then GONE else VISIBLE
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL ]
      [ textView $
          [ text $ (fromMaybe (getTitleConfig vehicleVariant).text state.data.serviceTierName) <> " "
          , color $ (getTitleConfig vehicleVariant).color
          , height WRAP_CONTENT
          , visibility if ((getLanguageLocale languageKey) == "ML_IN") then VISIBLE else GONE
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ getString $ TAXI_FROM_ZONE "TAXI_FROM_ZONE"
          , color Color.black800
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy]

  ]

navigateView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
navigateView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 44
  , background Color.white900
  , margin $ Margin 16 12 16 0
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , gravity CENTER
  , accessibility ENABLE
  , accessibilityHint $ (getEN $ GO_TO_ZONE "GO_TO_ZONE") <> " : Button"
  , accessibility DISABLE_DESCENDANT
  , visibility $ boolToVisibility $ isOtpRideFlow state && rideNotStarted state
  , onClick push $ const $ ShowDirections state.data.sourceLat state.data.sourceLng
  ][ imageView
     [ width $ V 20
     , height $ V 20
     , margin $ MarginRight 8
     , imageWithFallback $ fetchImage FF_ASSET "ic_navigation_blue"
     ]
   , textView $ 
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , gravity CENTER
     , text $ getString $ GO_TO_ZONE "GO_TO_ZONE"
     , color Color.blue900
     ] <> FontStyle.subHeading1 TypoGraphy
  ]

driverInfoView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoView push state =
  let tagConfig = specialZoneTagConfig state.props.zoneType.priorityTag
      rideStarted = not $ rideNotStarted state
      currentCityConfig = HU.getCityConfig  state.data.config.cityConfig (show state.props.merchantCity)
      brandingBannerViewVis = if currentCityConfig.iopConfig.enable then INVISIBLE else GONE
      airportZone = (state.props.zoneType.sourceTag == AIRPORT || state.props.zoneType.sourceTag == SPECIAL_PICKUP) && isJust state.data.addressWard
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility $ boolToVisibility $ not $ isOtpRideFlow state 
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , nestedScrollView true
      , scrollBarY false
      ][ linearLayout
         [ orientation VERTICAL
         , height WRAP_CONTENT
         , width MATCH_PARENT
         , background if state.props.zoneType.priorityTag /= NOZONE then tagConfig.backgroundColor else Color.grey900
         , gravity CENTER
         , cornerRadii $ Corners 24.0 true true false false
         , stroke $ state.data.config.driverInfoConfig.cardStroke
         ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , id $ getNewIDWithTag "driverInfoView"
              ][linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , background tagConfig.backgroundColor
                , cornerRadii $ Corners 24.0 true true false false
                , gravity CENTER
                , orientation HORIZONTAL
                , padding (PaddingVertical 6 6)
                , visibility $ boolToVisibility $ Array.any (_ == state.props.zoneType.priorityTag) [ METRO, SPECIAL_PICKUP ]
                , clickable $ isJust tagConfig.infoPopUpConfig
                , onClick push $ const $ SpecialZoneInfoTag
                ][imageView
                  [ width (V 15)
                  , height (V 15)
                  , margin (MarginRight 6)
                  , accessibility DISABLE
                  , imageWithFallback $ fetchImage COMMON_ASSET tagConfig.icon
                  ]
                , textView
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , textSize FontSize.a_14
                  , accessibility if state.props.zoneType.priorityTag == METRO then ENABLE else DISABLE
                  , accessibilityHint "Metro Ride"
                  , text tagConfig.text
                  , color Color.white900
                  , accessibility DISABLE
                  ]
                , imageView
                  [ width (V 18)
                  , height (V 18)
                  , visibility $ boolToVisibility $ isJust tagConfig.infoPopUpConfig
                  , margin (MarginLeft 6)
                  , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_white_info"
                  ]
                ]
              , linearLayout
                [ orientation VERTICAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                , background Color.grey700
                , gravity CENTER
                , cornerRadii $ Corners 24.0 true true false false
                ]$[ linearLayout
                  [ gravity CENTER
                  , background Color.transparentGrey
                  , height $ V 4
                  , width $ V 34
                  , accessibility ENABLE
                  , accessibilityHint $ "Bottom Sheet : Scrollable element : " <> if state.data.bottomSheetState == EXPANDED then "Scroll down to collapse details" else  "Scroll up to expand for more ride actions"
                  , margin $ MarginTop 8
                  , clickable true
                  , onClick push $ const ToggleBottomSheet
                  , cornerRadius if os == "IOS" then 2.0 else 4.0
                  ][]
                  , contactView push state airportZone
                  , airportPickupView push state airportZone
                  , if rideStarted then distanceView push state else dummyView push
                  , if rideStarted then maybe (dummyView push) (\item -> CarouselHolder.carouselView push $ getCarouselConfig item state) state.data.bannerData.bannerItem else dummyView push
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      , margin $ MarginTop if os == "IOS" && rideStarted then 12 else 0
                      ][addStopView push state
                      , rentalDetailsView push state
                      , driverDetailsView push (getDriverDetails state) "DriverDetailsView" "NumberPlate"
                      , paymentMethodView push state (getString FARE_ESTIMATE) true "PaymentMethodView"
                      , sizedBox (V 12) MATCH_PARENT
                      ]
                  ]
              ]
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , background Color.grey700
                ][if not state.data.config.showPickUpandDrop then dummyView push else sourceDestinationView push (getTripDetails state)
                , cancelRideLayout push state
                , brandingBannerView state.data.config.driverInfoConfig brandingBannerViewVis Nothing true ""
                ]
         ]
      ]
  ]

getCarouselConfig ∷ PrestoList.ListItem → DriverInfoCardState → CarouselHolder.CarouselHolderConfig BannerCarousel.PropConfig Action
getCarouselConfig view state = {
    view
  , items : BannerCarousel.bannerTransformer $ state.data.bannerArray
  , orientation : HORIZONTAL
  , currentPage : state.data.bannerData.currentPage
  , autoScroll : state.data.config.bannerCarousel.enableAutoScroll
  , autoScrollDelay : state.data.config.bannerCarousel.autoScrollDelay
  , id : "bannerCarousel"
  , autoScrollAction : Just UpdateBanner
  , onPageSelected : Just BannerChanged
  , onPageScrollStateChanged : Just BannerStateChanged
  , onPageScrolled : Nothing
  , currentIndex : state.data.bannerData.currentBanner
  , showScrollIndicator : true
  , layoutHeight : V 120
  , overlayScrollIndicator : false
}

distanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
distanceView push state = let 
  feature = state.data.config.feature
  in
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay FunctionCall)] $ 
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , onAnimationEnd push $ const $ NoAction
  , padding $ Padding 16 8 16 (if Array.length state.data.bannerArray > 0 then 0 else 14)
  ][linearLayout
    [ height WRAP_CONTENT
    , weight 1.0
    , accessibility ENABLE
    , accessibilityHint $ getEN ENJOY_THE_RIDE
    , orientation VERTICAL
    ][ textView $
       [ text $ getTitleText
       , color Color.black900
      --  , ellipsize true
       , width MATCH_PARENT
       , singleLine false
      --  , maxLines 2
       ] <> FontStyle.body7 TypoGraphy
     , etaView push state
     , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , color Color.blue900
        , text $ getString TRACK_ON_GOOGLE_MAP
        , onClick push $ const $ OnNavigate DRIVE state.data.destinationLat state.data.destinationLng
        ] <> FontStyle.body6 TypoGraphy
     ]
  , if state.props.isChatWithEMEnabled then chatButtonView push state else dummyView push
  ]
  where 
    getTitleText :: String
    getTitleText = do
      case state.data.fareProductType of
        FPT.RENTAL -> getRentalTitleText
        FPT.DELIVERY -> if state.data.destinationReached then getString ARRIVED_AT_DROP else getString OUT_FOR_DELIVERY 
        _ -> getString ENJOY_THE_RIDE
    getRentalTitleText :: String
    getRentalTitleText = 
      if (null state.data.rentalData.startTimeUTC) then (getString ENJOY_THE_RIDE)
      else 
        let startTime = state.data.rentalData.startTimeUTC
            startTimeNotEmpty = startTime /= ""
            baseDuration = state.data.rentalData.baseDuration            
            endUTC = if startTimeNotEmpty then  runFn2 getUTCAfterNSecondsImpl startTime (baseDuration * 60 * 60) else ""
            endTimeInHH = if startTimeNotEmpty then convertUTCtoISC endUTC "h" <> ":" <> convertUTCtoISC endUTC "mm" <> " " <> convertUTCtoISC endUTC "A" else ""
        in if state.data.destination /= "" then getString RENTAL_RIDE_UNTIL <> " " <> endTimeInHH
          else "Add Stop to continue ride" <> (if state.props.endOTPShown then " or share End-OTP to end ride" else "")

etaView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
etaView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop 4
  , accessibility ENABLE
  , cornerRadius 16.0
  , background Color.blue800
  , accessibilityHint $ "arriving in " <> fromMaybe "" state.data.estimatedTimeToReachDestination
  , visibility $ boolToVisibility $ state.data.fareProductType == FPT.DELIVERY && state.data.estimatedTimeToReachDestination /= Nothing
  ][ textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , margin $ Margin 8 2 8 2
    , color Color.white900
    , text $ getString ESTIMATED_ARRIVAL_BY <> " " <> fromMaybe "" state.data.estimatedTimeToReachDestination
    ] <> FontStyle.body1 TypoGraphy
  ]

brandingBannerView :: forall w. DriverInfoConfig -> Visibility -> Maybe String -> Boolean -> String -> PrestoDOM (Effect Unit) w
brandingBannerView driverInfoConfig isVisible uid onUsRide providerName = 
  let providerRideText = getString if onUsRide then GUARANTEED_RIDE else THIS_RIDE_FULFILLED_BY providerName
      style = if onUsRide then FontStyle.captions else FontStyle.body3
  in 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    , gravity BOTTOM
    , visibility $ isVisible
    ][ separator (MarginTop 0) (V 1) Color.grey900 true
     , linearLayout
       ([ width MATCH_PARENT
       , height WRAP_CONTENT
       , gravity CENTER
       , background driverInfoConfig.footerBackgroundColor
       , padding $ Padding 12 12 12 (12+safeMarginBottom)
       ] <> if isJust uid then [id $ getNewIDWithTag $ fromMaybe "" uid] else [])
       [ imageView
          [ imageWithFallback $ driverInfoConfig.footerImageUrl
          , height $ V 20
          , width $ V 62
          , margin $ MarginHorizontal 10 10
          , visibility $ boolToVisibility $ onUsRide
          ]
        , textView $ 
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin $ MarginLeft 5
          , text providerRideText
          ] <> style TypoGraphy
      ]
    ]

cancelRideLayout :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
cancelRideLayout push state =
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay FunctionCall)] $ 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , onAnimationEnd push $ const $ NoAction
  , margin $ if state.data.config.showPickUpandDrop then MarginTop 0 else MarginTop 12
  , padding $ PaddingBottom if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0
  , visibility $ boolToVisibility $ showCancelRideCTA state
  ][ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ Padding 10 14 10 16
    , accessibilityHint "Cancel Ride : Button"
    , accessibility ENABLE
    , margin $ if os == "IOS" then MarginVertical 0 24 else MarginVertical 2 8
    , onClick push $ const $ CancelRide state
    , rippleColor Color.rippleShade
    , cornerRadius 20.0
    ][ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black700
      , textFromHtml $ "<u>" <> (getString CANCEL_RIDE) <> "</u>"
      , alpha $ if (getMerchant FunctionCall) == MOBILITY_PM then 0.54 else 1.0
      ] <> FontStyle.body1 TypoGraphy
    ]
  ]

---------------------------------- contactView ---------------------------------------
contactView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> Boolean -> PrestoDOM (Effect Unit) w
contactView push state airportZone =
  linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 4 16 8
    , visibility $ boolToVisibility $ rideNotStarted state
    ][ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER_VERTICAL
        ][ linearLayout
            [ width (V (((screenWidth unit)/3 * 2)-27))
            , height WRAP_CONTENT
            , accessibilityHint $ getAccessibilityHintText
            , accessibility ENABLE
            ][  textView $
                [ text $ driverPickUpStatusText state eta
                , color Color.black900
                ] <> FontStyle.body7 TypoGraphy
              ]
          , textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , color Color.blue900
              , text $ getString SHOW_WALKING_DIRECTION
              , accessibilityHint "Show walking direction : Button"
              , visibility $ boolToVisibility $ not airportZone
              , onClick push $ const $ ShowDirections state.data.sourceLat state.data.sourceLng
              ] <> FontStyle.body6 TypoGraphy
         ]
      , textView [weight 1.0]
      , chatButtonView push state
    ]
    where 
      eta = secondsToHms $ fromMaybe 0 state.data.eta
      distance = if state.data.distance < 1000 then show state.data.distance <> " meters" else fromMetersToKm state.data.distance
      getAccessibilityHintText = "Ride Status : " <> 
        if distance /= "0 meters" 
          then if eta /= "--" 
                then state.data.driverName <> " is " <> distance <> " Away and is arriving in " <> eta
                else if state.data.waitingTime /= "--" 
                      then state.data.driverName <> " is waiting for you."
                      else state.data.driverName <> " is " <> distance <> "away"
          else state.data.driverName <> " is waiting for you."

airportPickupView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> Boolean -> PrestoDOM (Effect Unit) w
airportPickupView push state airportZone = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , onClick push $ const $ ShowDirections state.data.sourceLat state.data.sourceLng
    , accessibilityHint "Show walking direction : Button"
    , visibility $ boolToVisibility $ airportZone && rideNotStarted state
    , background Color.blue600
    , stroke $ "1,"<> Color.blue700
    , padding $ Padding 8 8 8 8
    , margin $ Margin 16 5 16 12
    , cornerRadius 8.0
    , gravity CENTER_VERTICAL
    ][  PrestoAnim.animationSet[ Anim.triggerOnAnimationEnd true] $ 
        lottieAnimationView
        [ height $ V 24
        , width $ V 24
        , gravity CENTER
        , id $ getNewIDWithTag "WalkingLottie"
        , margin $ MarginRight 4
        , onAnimationEnd (\action -> do
          void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig {speed = 1.0, rawJson = userCommonAssetBaseUrl <> "lottie/walk_anim.json", lottieId = getNewIDWithTag "WalkingLottie", repeat = true}
          push action
          ) (const NoAction)
        ]
      , textView $
        [ weight 1.0
        , height WRAP_CONTENT
        , color Color.blue900
        , text $ getString $ WALK_TO $ fromMaybe "" state.data.addressWard
        ] <> FontStyle.body6 TypoGraphy
      , imageView
        [ width $ V 14
        , height $ V 14
        , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_right_arrow_blue_2"
        ]
    ]


chatButtonView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
chatButtonView push state = 
  linearLayout
  [ width WRAP_CONTENT
  , layoutGravity "right"
  , height WRAP_CONTENT
  ]
  [ linearLayout
    [ height $ V 40
    , width $ V 64
    , gravity CENTER
    , cornerRadius if os == "IOS" then 20.0 else 32.0
    , background state.data.config.driverInfoConfig.callBackground
    , stroke state.data.config.driverInfoConfig.callButtonStroke
    , onClick push $ const $ MessageDriver
    , accessibilityHint "Chat and Call : Button"
    , accessibility ENABLE
    , rippleColor Color.rippleShade
    ][ imageView
      [ imageWithFallback imageAsset
      , height $ V state.data.config.driverInfoConfig.callHeight
      , width $ V state.data.config.driverInfoConfig.callWidth
      ]
    ]
  ]
  where 
    feature = state.data.config.feature
    imageAsset = case feature.enableChat && disableChat state.data.fareProductType, state.data.providerType of
      true, ONUS -> fetchImage FF_ASSET if state.props.unReadMessages then "ic_chat_badge_green" else "ic_call_msg"
      _, _ -> fetchImage FF_COMMON_ASSET "ny_ic_call"


---------------------------------- ratingView ---------------------------------------

ratingView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
ratingView push state =
  linearLayout
  [ orientation HORIZONTAL
  , margin $ MarginTop 40
  , height $ V 19
  , width $ V 50
  , padding $ Padding 6 3 6 3
  , background state.data.config.driverInfoConfig.ratingBackground
  , gravity CENTER_VERTICAL
  , stroke  state.data.config.driverInfoConfig.ratingStroke
  , cornerRadius state.data.config.driverInfoConfig.ratingCornerRadius
  , accessibility DISABLE
  ][textView $
    [ text $ if state.data.rating == 0.0 then (getString NEW_) else show state.data.rating
    , color state.data.config.driverInfoConfig.ratingTextColor
    , gravity CENTER_VERTICAL
    , margin $ MarginLeft if os == "IOS" then 0 else 3
    , color Color.black700
    , accessibility DISABLE
    ] <> FontStyle.body16 TypoGraphy
  , imageView
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_star_active_rounded"
    , height $ V 11
    , width $ V 11
    , margin $ MarginLeft 3
    , accessibility DISABLE
    ]
  ]

---------------------------------- paymentMethodView ---------------------------------------

paymentMethodView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> String -> Boolean -> String -> PrestoDOM (Effect Unit) w
paymentMethodView push state title shouldShowIcon uid =
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , id $ getNewIDWithTag uid
  , margin $ Margin 16 12 16 0
  , background Color.white900
  , padding $ Padding 16 16 16 16
  , accessibility ENABLE
  , accessibilityHint $ "Fare Estimate :" <> state.data.config.currency <> show state.data.price <> " : Pay by cash or U P I"
  , cornerRadius 8.0
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , accessibility DISABLE_DESCENDANT
      ][  linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , padding $ Padding 0 5 20 5
          , onClick push $ const RateCardInfo
          , clickable state.props.isRateCardAvailable 
          ][ textView $
             [ text title
             , color Color.black700
             ] <> FontStyle.body3 TypoGraphy
          , imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
            , height $ V 16
            , width $ V 16
            , visibility $ boolToVisibility state.props.isRateCardAvailable 
            , margin $ MarginLeft 2
            ]
          ]
        , textView $
          [ text $ state.data.config.currency <> show state.data.price
          , margin $ MarginTop 4
          , color Color.black800
          , width MATCH_PARENT
          ] <> FontStyle.h2 TypoGraphy
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ][]
      , linearLayout[
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER 
        , orientation VERTICAL
        ][
          extraChargesView
        , linearLayout
            [ orientation HORIZONTAL
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER
            , visibility $ boolToVisibility shouldShowIcon
            , margin $ MarginTop 4
            ][  imageView
                [ imageWithFallback $ fetchImage FF_ASSET  "ny_ic_wallet_filled"
                , height $ V 20
                , width $ V 20
                ]
              , textView $
                [ text $ getString PAY_BY_CASH_OR_UPI
                , color Color.black700
                , padding $ PaddingLeft 4
                ] <> FontStyle.body3 TypoGraphy
              ]
          ]
      ]
  where 
    extraChargesView = 
      linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER_HORIZONTAL
      , margin $ MarginTop 4
      , visibility $ boolToVisibility state.data.hasToll
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , padding $ Padding 8 8 8 8
        , background Color.grey700
        , orientation HORIZONTAL
        , cornerRadius 16.0
        , gravity CENTER_VERTICAL
        ][
          imageView[
            height $ V 16
          , width $ V 16
          , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_black_toll"
          , margin $ MarginRight 4
          ]
        , textView $ [ 
            textFromHtml $ getString TOLL_CHARGES_INCLUDED
            , color Color.black800
            , gravity CENTER_HORIZONTAL
            , height WRAP_CONTENT
            , gravity CENTER_HORIZONTAL
            , width MATCH_PARENT
            ] <> FontStyle.tags TypoGraphy
        ]
      ] 



specialZoneShimmerView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
specialZoneShimmerView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , background Color.white900
  , padding $ PaddingHorizontal 16 16
  , cornerRadii $ Corners 24.0 true true false false
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginVertical 8 12
    ][linearLayout
      [ gravity CENTER
      , background Color.transparentGrey
      , height $ V 4
      , width $ V 34
      , cornerRadius if os == "IOS" then 2.0 else 4.0
      ][] 
    ]
    , linearLayout
      [ height $ V 40
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ customTextView (if rideNotStarted state then 40 else 20) ((screenWidth unit) / 10 * 6) 0]
      , linearLayout
        [ width $ MATCH_PARENT
        , height $ V 44
        , visibility $ boolToVisibility $ rideNotStarted state
        , margin $ MarginVertical 4 12
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
        ][ customTextView 20 ((screenWidth unit) / 10 * 6) 0]
      , if not $ rideNotStarted state then driverInfoShimmer else dummyView push
      , paymentMethodShimmer push state
      , addressShimmerView
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginVertical 12 16
        ][ customTextView 20 80 0 ]
    ]

shimmerView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
shimmerView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , background Color.grey700
  , padding $ PaddingHorizontal 16 16
  , cornerRadii $ Corners 24.0 true true false false
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginVertical 8 12
    ][linearLayout
      [ gravity CENTER
      , background Color.transparentGrey
      , height $ V 4
      , width $ V 34
      , cornerRadius if os == "IOS" then 2.0 else 4.0
      ][] 
    ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ customTextView 20 ((screenWidth unit) / 10 * 6) 0
       , linearLayout [weight 1.0][]
       , shimmerFrameLayout
         [ height $ V 40
         , width $ V 64
         , cornerRadius 52.0
         , visibility $ boolToVisibility $ rideNotStarted state
         ][linearLayout
           [ height $ V 40
           , width $ V 64
           , cornerRadius 52.0 
           , background Color.grey900
           ][]
         ]
      ]
      , driverInfoShimmer
      , paymentMethodShimmer push state
      , addressShimmerView
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginVertical 12 16
        ][ customTextView 20 80 0 ]
    ]

customTextView :: forall w. Int -> Int -> Int -> PrestoDOM (Effect Unit) w
customTextView height' width' bottomMargin' =
  shimmerFrameLayout
  [ cornerRadius 8.0
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , background Color.grey900
    , margin $ MarginBottom bottomMargin'
    , cornerRadius 8.0
    ][]
  ]

sfl :: forall w. Int -> Int -> Number -> PrestoDOM (Effect Unit) w
sfl height' width' radius' =
  shimmerFrameLayout
  [ cornerRadius radius'
  , stroke $ "1," <> Color.grey900
  , margin $ MarginBottom 12
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , cornerRadius radius'
    , background Color.grey900
    ][]
  ]

paymentMethodShimmer :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
paymentMethodShimmer push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , margin $ MarginBottom 12
  , padding $ Padding 16 16 16 16
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , gravity CENTER
  ][linearLayout
    [ height $ WRAP_CONTENT
    , width $ V ((screenWidth unit) / 2)
    , orientation VERTICAL
    ][ customTextView 16 80 4
     , customTextView 16 40 4
    ]
   , linearLayout[weight 1.0][]
   , customTextView 16 120 4
  ]

---------------------------------- separator ---------------------------------------

separator :: forall w. Margin -> Length -> String -> Boolean -> PrestoDOM (Effect Unit) w
separator margin' height' color' isVisible =
  linearLayout
  [ height $ height'
  , margin $ margin'
  , width MATCH_PARENT
  , visibility if isVisible then VISIBLE else GONE
  , background color'
  ][]

---------------------------------- primaryButtonConfig ---------------------------------------

primaryButtonConfig :: PrimaryButton.Config
primaryButtonConfig = let
    config' = PrimaryButton.config
    primaryButtonConfig' = config'
      { width = WRAP_CONTENT
      , height = WRAP_CONTENT
      , background = Color.mint
      , cornerRadius = 17.0
      , isPrefixImage = true
      , prefixImageConfig {
          height = V 18
        , width = V 18
        , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_call"
        , margin = Margin 20 10 20 10
        }
      , id = "CallButton"
      }
  in primaryButtonConfig'


---------------------------------- sourceToDestinationConfig ---------------------------------------

sourceToDestinationConfig :: DriverInfoCardState -> SourceToDestination.Config
sourceToDestinationConfig state = let
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = Margin 16 0 40 0
    , id = Just "DriverInfoCardSTDC"
    , overrideSeparatorCount = 6
    , separatorMargin = 19
    , sourceImageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_pickup"
      , height = V 14
      , width = V 14
      }
    , sourceTextConfig {
        text = state.data.source
      , textStyle = FontStyle.Body1
      , ellipsize = true
      , margin = MarginLeft 10
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl =fetchImage FF_ASSET "ny_ic_drop"
      , height = V 14
      , width = V 14
      }
    , destinationTextConfig {
        text = state.data.destination
      , maxLines = 1
      , textStyle = FontStyle.Body1
      , margin = MarginLeft 10
      , ellipsize = true
      }
    , distanceConfig {
        distanceVisibility = VISIBLE
      , distanceValue = state.data.estimatedDistance <> " km"
      , background = Color.grey700
  }
    }
  in sourceToDestinationConfig'

destinationView ::  forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
destinationView push state=
  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , padding $ Padding 16 16 16 16
      , margin $ MarginBottom (if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0)
      , width WRAP_CONTENT
      , gravity LEFT
      ][  textView $
          [ text if true then "Drop :-" else  getString RIDE_FARE
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text state.data.destination
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginTop 4
            ][ textView (
                [ text $ state.data.estimatedDistance <> " km"
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
              , linearLayout
                [height $ V 4
                , width $ V 4
                , cornerRadius 2.5
                , background Color.black600
                , margin (Margin 6 2 6 0)
                ][]
              , textView (
                [ text state.props.estimatedTime
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
            ]
      ]
openGoogleMap :: forall w . (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
openGoogleMap push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity LEFT
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.blue900
      , padding $ Padding 20 15 20 15
      , margin $ MarginRight 16
      , cornerRadius 30.0
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push $ const $ OnNavigate DRIVE state.data.destinationLat state.data.destinationLng
      ][ textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString NAVIGATE)
          , gravity CENTER
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
          )
        , imageView
          [ width $ V 20
          , height $ V 20
          , margin (MarginLeft 6)
          , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_navigation"
          ]
      ]
  ]

dummyView :: forall w . (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dummyView push  =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]

configurations ∷ { letterSpacing ∷ Number , paddingOTP ∷ Padding , paddingOTPText ∷ Padding }
configurations =
  case os of
    "IOS" -> {paddingOTPText : PaddingVertical 4 4
              , letterSpacing : 6.0
              , paddingOTP : Padding 20 5 18 7}
    _     -> {paddingOTPText : PaddingVertical 2 2
              , letterSpacing : 3.0
              , paddingOTP : Padding 11 0 11 7
              }

getAnimationDelay :: LazyCheck -> Int
getAnimationDelay dummy = 100

getDriverDetails :: DriverInfoCardState -> DriverDetailsType
getDriverDetails state = {
  rating : state.data.rating
  , driverName : state.data.driverName
  , vehicleDetails : state.data.vehicleDetails
  , vehicleVariant : state.data.vehicleVariant
  , merchantCity : state.props.merchantCity
  , registrationNumber : state.data.registrationNumber
  , config : state.data.config
  , rideStarted : state.props.currentStage == RideStarted
  , enablePaddingBottom : false
  , vehicleModel : capitalize $ STR.toLower state.data.vehicleModel
  , vehicleColor : capitalize $ STR.toLower state.data.vehicleColor
  , serviceTierName : state.data.serviceTierName
  , providerType : state.data.providerType
  , showAcView : state.data.cityConfig.enableAcViews
  , fareProductType : state.data.fareProductType
  , isAirConditioned : state.data.isAirConditioned
  , isOtpRideFlow : state.props.isOtpRideFlow
}

endOTPAnimConfig :: DriverInfoCardState -> AnimConfig.AnimConfig
endOTPAnimConfig state =
  if os == "IOS" 
    then
      AnimConfig.animConfig
      { fromX = -30
      , toX = 0
      , duration = 1500
      , ifAnim = state.props.showEndOTP
      }
    else
      AnimConfig.animConfig
      { fromX =  -20
      , toX =  0
      , duration = 1500
      , ifAnim = state.props.showEndOTP
      }

getTripDetails :: DriverInfoCardState -> TripDetails Action 
getTripDetails state = {
  rideStarted : state.props.currentStage == RideStarted
  , source : state.data.source
  , destination : state.data.destination
  , onAnimationEnd : NoAction
  , backgroundColor : Color.white900
  , enablePaddingBottom : true
  , fareProductType : state.data.fareProductType
  , enableEditDestination : state.data.config.feature.enableEditDestination && not isHybridFlowParcel
  , editingDestinationLoc : EditingLocation ST.Destination
  , rideAccepted : state.props.currentStage == RideAccepted
  , editingPickupLocation : EditingLocation ST.Source
  , isEditPickupEnabled : state.data.config.feature.enableEditPickupLocation && not isHybridFlowParcel
  , isOtpRideFlow : state.props.isOtpRideFlow
  , senderDetails : state.data.senderDetails
  , receiverDetails : state.data.receiverDetails
  }
  where 
    isHybridFlowParcel = state.data.fareProductType == FPT.DELIVERY && HU.isParentView FunctionCall

driverPickUpStatusText :: DriverInfoCardState -> String -> String
driverPickUpStatusText state _ = 
  case state.data.fareProductType of
    FPT.DELIVERY -> if state.data.driverArrived then getString PICKUP_IN_PROGRESS else getString OUT_FOR_PICKUP
    _ -> case state.props.zoneType.priorityTag of
          SPECIAL_PICKUP -> getString DRIVER_AT_PICKUP_LOCATION
          _ -> if state.data.waitingTime == "--" then getString DRIVER_IS_ON_THE_WAY else getString DRIVER_IS_WAITING_AT_PICKUP 
     


rideNotStarted :: DriverInfoCardState -> Boolean
rideNotStarted state = 
  let lastStage = if state.props.isChatWithEMEnabled then RideStarted else RideAccepted
  in Array.any (_ == state.props.currentStage) [RideAccepted, ChatWithDriver] && (lastStage == RideAccepted || state.props.stageBeforeChatScreen == RideAccepted)

rentalDetailsView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
rentalDetailsView push state =
  let rentalData = state.data.rentalData
      isRideStarted = state.props.currentStage == RideStarted
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0
  , padding $ Padding 16 16 16 16
  , afterRender push $ const NoAction
  , margin $ Margin 16 12 16 12
  , background Color.white900
  , visibility $ boolToVisibility $ state.data.fareProductType == FPT.RENTAL
  ][  rideInfoPill state {title : getString TIME , value :(if isRideStarted then state.props.rideDurationTimer else "0") <> " hr", perUnit : " / " <> show rentalData.baseDuration <> " hr"}
    , separatorView true
    , rideInfoPill state {title : getString DISTANCE, value : show rentalData.baseDistance <> " km", perUnit : "" }
    ]

rentalTimeView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> STR -> Boolean -> PrestoDOM (Effect Unit) w
rentalTimeView push state showText showInfo =
  let rentalData = state.data.rentalData
      isRideStarted = state.props.currentStage == RideStarted
  in 
    PrestoAnim.animationSet [ fadeIn true] $
    linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity $ if isTime showText then LEFT else CENTER
    , orientation VERTICAL
    ]
    [ linearLayout[height WRAP_CONTENT
      , width WRAP_CONTENT][textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ getString showText
      , color Color.black650
      , singleLine true
      ] <> FontStyle.body3 TypoGraphy
    , imageView 
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_blue_lg"
            , height $ V 15
            , visibility $ boolToVisibility $ showInfo
            , width $ V 15 
            , margin $ MarginLeft 4 
            , onClick push $ const $ RentalInfo
            ]]
    , linearLayout
      [ height MATCH_PARENT
      , width $ if isTime showText then WRAP_CONTENT else MATCH_PARENT
      , orientation HORIZONTAL
      ] 
      [ textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ case showText of 
                TIME -> (if isRideStarted then state.props.rideDurationTimer else "0") <> " hr"
                DISTANCE -> show rentalData.baseDistance <> " km"
                _ -> if rentalData.startOdometer == "" then "-" else rentalData.startOdometer <> " km"
            , color Color.black800
            ] <> FontStyle.body1 TypoGraphy

      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , visibility $ boolToVisibility $ isTime showText
        , text $ " / " <> show rentalData.baseDuration <> " hr"
        , color Color.black600
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]
    where
      isTime :: STR -> Boolean
      isTime str =
        case str of
          TIME -> true
          _ -> false

addStopView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
addStopView push state =
  let isDestinationTextGiven = state.data.destination /= ""
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 0 16 0
    , padding $ Padding 16 12 16 12
    , background Color.white900
    , orientation VERTICAL
    , cornerRadius 8.0
    , visibility $ boolToVisibility $ state.data.fareProductType == FPT.RENTAL
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ]
      [ imageView 
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_blue_circle"
        , height $ V 8
        , width $ V 8  
        , margin $ MarginRight 8
        ]
      , textView $
        [ text $ getString NEXT_STOP
        ] <> FontStyle.body3 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , margin $ MarginTop 2
      , gravity CENTER_VERTICAL
      ]
      [ textView $
        [ text $ if isDestinationTextGiven then state.data.destination else getString NOT_ADDED_YET
        , ellipsize true
        , singleLine true
        , color Color.black800
        , height WRAP_CONTENT
        , weight 1.0
        , gravity LEFT
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ text $ if isDestinationTextGiven then getString EDIT else getString ADD_NOW
        , color Color.blue800
        , onClick push $ const AddStop
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , accessibility ENABLE
        , accessibilityHint  $ if isDestinationTextGiven then "Edit : Button" else "Add now : Button" 
        , padding $ Padding 16 4 16 4
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]

separatorView :: forall w . Boolean -> PrestoDOM (Effect Unit) w
separatorView visibility' =
  linearLayout
    [ height MATCH_PARENT
    , gravity CENTER
    , width WRAP_CONTENT
    , margin $ MarginHorizontal 32 32
    , visibility $ boolToVisibility $ visibility'
    ][ linearLayout
      [ width $ V 1
      , background Color.lightGrey
      , height MATCH_PARENT
      ][]
    ]

rideInfoPill state rideData = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT 
    , orientation VERTICAL 
    ][  textView $
        [ text $ rideData.title
        , color Color.black700 
        ] <> FontStyle.body24 TypoGraphy
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT 
        ][  
          textView $ 
          [ text $ rideData.value
          , color Color.black800 
          ] <> FontStyle.body24 TypoGraphy
        , textView  $ 
          [ text $ rideData.perUnit
          , color Color.black600 
          ] <> FontStyle.body24 TypoGraphy
        ]
      
    ]

isOtpRideFlow :: DriverInfoCardState -> Boolean
isOtpRideFlow state = state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || (state.props.isOtpRideFlow && state.props.currentStage == RideAccepted)

showCancelRideCTA :: DriverInfoCardState -> Boolean
showCancelRideCTA state = rideNotStarted state && HU.isDeliveryInitiator state.data.requestorPartyRoles