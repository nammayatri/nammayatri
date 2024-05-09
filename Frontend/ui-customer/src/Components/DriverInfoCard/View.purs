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

import Animation (fadeIn, fadeInWithDelay, scaleYAnimWithDelay, shimmerAnimation)
import CarouselHolder as CarouselHolder
import Common.Styles.Colors as CommonColor
import Common.Types.App (LazyCheck(..))
import Components.BannerCarousel as BannerCarousel
import Components.DriverInfoCard.Controller (Action(..), DriverInfoCardState)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Function.Uncurried (runFn1)
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), split, length, take, drop, replaceAll, Replacement(..), contains, toLower)
import Data.String as STR
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, safeMarginBottom, screenWidth)
import Engineering.Helpers.Suggestions (getMessageFromKey, chatSuggestion)
import Engineering.Helpers.Utils (showAndHideLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl, getPaymentMethod, secondsToHms, makeNumber, getVariantRideType, getTitleConfig, getCityNameFromCode, getDefaultPixelSize, getCurrencySymbol)
import Helpers.SpecialZoneAndHotSpots (specialZoneTagConfig)
import Helpers.Utils (parseFloat)
import JBridge (fromMetersToKm, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types (DriverInfoConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility, capitalize)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, negate, (>), (<), (-), (*), bind, pure, discard, not, (&&), (||), (/=), (+), (+))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (BottomSheetState(..), Accessiblity(..), Gradient(..), Shadow(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, alignParentLeft, alignParentRight, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, shimmerFrameLayout, rippleColor, clipChildren, shadow, clipToPadding, rotation, horizontalScrollView, disableKeyboardAvoidance, scrollBarX)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.List as PrestoList
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.Localizable.EN (getEN)
import Screens.Types (Stage(..), ZoneType(..), SearchResultType(..), SheetState(..), City(..), NavigationMode(..))
import Storage (KeyStore(..))
import Storage ( getValueToLocalStore, KeyStore(..)) as STO
import Styles.Colors as Color
import Common.Styles.Colors as CommonColor
import Engineering.Helpers.Utils (showAndHideLoader)
import Types.App (defaultGlobalState)
import Helpers.Utils as HU

view :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  let enableShareRide = state.data.config.feature.enableShareRide && (not $ rideNotStarted state)
      enableSupport = state.data.config.feature.enableSupport && (Array.any (_ == state.props.currentStage) ) [RideAccepted, RideStarted, ChatWithDriver] 
  in
  linearLayout
  [ height WRAP_CONTENT
  , width $ V (screenWidth unit)
  , background Color.transparent
  , orientation VERTICAL
  , id $ getNewIDWithTag "BottomSheetLayout"
  , afterRender push $ const $ NoAction
  ][ linearLayout
     [ height $ WRAP_CONTENT
     , width MATCH_PARENT
     , accessibility DISABLE
     , clipChildren false
     , id $ getNewIDWithTag "DriverInfoCardActionView"
     ][ otpAndWaitView push state 
      , linearLayout[weight 1.0][]
      , if enableShareRide then shareRideButton push state else if enableSupport then contactSupport push state else dummyView push -- TEMP FIX UNTIL THE NEW DESIGN IS DONE
     ]
  , driverInfoViewSpecialZone push state
  , driverInfoView push state
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
  , visibility if state.props.currentSearchResultType == QUOTES then VISIBLE else GONE
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
              , driverDetailsView (getDriverDetails state) "SpecialDriverDetailsView" "SpecialNumberPlate"
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
  [ height $ WRAP_CONTENT
  , width $ WRAP_CONTENT
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

otpAndWaitView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
otpAndWaitView push state =
  horizontalScrollView
  [ height $ V 56
  , width $ V $ (screenWidth unit) - 56
  , disableKeyboardAvoidance true
  , scrollBarX false
  , scrollBarY false
  , gravity CENTER
  , accessibility DISABLE
  ][ linearLayout
     [ height$ V 56
     , width MATCH_PARENT
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
     ] <> if (state.props.currentSearchResultType == QUOTES || state.data.driverArrived) then 
           [(PrestoAnim.animationSet [ fadeIn true ] $ 
           let isQuotes = state.props.currentSearchResultType == QUOTES
           in
           linearLayout
           [ width WRAP_CONTENT
           , height $ V 40
           , visibility $ boolToVisibility $ rideNotStarted state
           , cornerRadius if os == "IOS" then 20.0 else 32.0
           , background Color.white900
           , clickable true
           , onClick push $ const WaitingInfo
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
  [ height MATCH_PARENT
  , width MATCH_PARENT
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
waitTimeHint state = (if state.props.currentSearchResultType == QUOTES then "O T P Expires in : " else "Wait Time : ") <> case STR.split (STR.Pattern ":") state.data.waitingTime of
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
          threshold = if state.props.currentSearchResultType == QUOTES then mins < 5 else mins > 2
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
  , visibility $ boolToVisibility $ not $ rideNotStarted state
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
  ][ if rideNotStarted state then specialZoneHeader $ STO.getValueToLocalStore STO.SELECTED_VARIANT
     else distanceView push state
  ]

specialZoneHeader :: forall w. String -> PrestoDOM ( Effect Unit) w
specialZoneHeader vehicleVariant =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 16 16
  , margin $ MarginTop 6
  , accessibility ENABLE
  , accessibilityHint $ "Board the first" <> (getTitleConfig vehicleVariant).text <> (getEN $ TAXI_FROM_ZONE "TAXI_FROM_ZONE")
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
          [ text $ (getTitleConfig vehicleVariant).text <> " "
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
          [ text $ (getTitleConfig vehicleVariant).text <> " "
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
  , visibility $ boolToVisibility $ state.props.currentSearchResultType == QUOTES && rideNotStarted state
  , onClick push $ const $ OnNavigate WALK state.data.sourceLat state.data.sourceLng
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
  let tagConfig = specialZoneTagConfig state.props.zoneType
      rideStarted = not $ rideNotStarted state
      currentCityConfig = HU.getCityConfig  state.data.config.cityConfig (show state.props.merchantCity)
      brandingBannerViewVis = if currentCityConfig.iopConfig.enable then INVISIBLE else GONE
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility if state.props.currentSearchResultType == QUOTES then GONE else VISIBLE
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
         [ orientation VERTICAL
         , height WRAP_CONTENT
         , width MATCH_PARENT
         , background if state.props.zoneType /= NOZONE then tagConfig.backgroundColor else Color.grey900
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
                , visibility $ boolToVisibility $ Array.any (_ == state.props.zoneType) [ METRO, SPECIAL_PICKUP ]
                , clickable $ isJust tagConfig.infoPopUpConfig
                , onClick push $ const $ SpecialZoneInfoTag
                ][imageView
                  [ width (V 15)
                  , height (V 15)
                  , margin (MarginRight 6)
                  , accessibility DISABLE
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET tagConfig.icon
                  ]
                , textView
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , textSize FontSize.a_14
                  , accessibility if state.props.zoneType == METRO then ENABLE else DISABLE
                  , accessibilityHint "Metro Ride"
                  , text tagConfig.text
                  , color Color.white900
                  ]
                , imageView
                  [ width (V 18)
                  , height (V 18)
                  , visibility $ boolToVisibility $ isJust tagConfig.infoPopUpConfig
                  , margin (MarginLeft 6)
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_info"
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
                  , contactView push state
                  , if rideStarted then distanceView push state else dummyView push
                  , if rideStarted then maybe (dummyView push) (\item -> CarouselHolder.carouselView push $ getCarouselConfig item state) state.data.bannerData.bannerItem else dummyView push
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      , margin $ MarginTop if rideStarted then 12 else 0
                      ][driverDetailsView (getDriverDetails state) "DriverDetailsView" "NumberPlate"
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
}

distanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
distanceView push state = 
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay FunctionCall)] $ 
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , onAnimationEnd push $ const $ NoAction
  , padding $ Padding 16 8 16 14
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , accessibility ENABLE
    , accessibilityHint $ getEN ENJOY_THE_RIDE
    , orientation VERTICAL
    ][ textView $
       [ text $ getString ENJOY_THE_RIDE
       , color Color.black900
       , ellipsize true
       , singleLine true
       ] <> FontStyle.body7 TypoGraphy
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
  , visibility $ boolToVisibility $ rideNotStarted state
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
contactView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
contactView push state =
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
            , accessibilityHint $ "Ride Status : " <> if eta /= "--" then (state.data.driverName <> " is " <> eta <> " Away") else if state.data.waitingTime == "--" then (state.data.driverName <> " is on the way") else (state.data.driverName <> " is waiting for you.") 
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
            , visibility $ boolToVisibility $ state.props.zoneType == SPECIAL_PICKUP
            , onClick push $ const $ OnNavigate WALK state.data.sourceLat state.data.sourceLng
            ] <> FontStyle.body6 TypoGraphy
         ]
      , chatButtonView push state
    ]
    where eta = secondsToHms (fromMaybe 0 state.data.eta)

chatButtonView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
chatButtonView push state = 
  linearLayout
    [ width MATCH_PARENT
    , gravity RIGHT
    , height WRAP_CONTENT
    ][linearLayout
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
      imageAsset = case feature.enableChat, state.data.providerType of
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
  let currency = getCurrencySymbol state.data.currency
  in
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
  , accessibilityHint $ "Fare Estimate :" <> currency <> show state.data.price <> " : Pay by cash or U P I"
  , cornerRadius 8.0
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , accessibility DISABLE_DESCENDANT
      ][  textView $
          [ text title
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ currency <> show state.data.price
          , margin $ MarginTop 4
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ "(" <> (getString TOLL_CHARGES_INCLUDED) <> ")"
          , color Color.black900
          , margin $ MarginTop 4
          , visibility $ boolToVisibility $ (STO.getValueToLocalStore STO.HAS_TOLL_CHARGES) == "true" -- @TODO: Change once backend was DONE
          ] <> FontStyle.body3 TypoGraphy
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ][]
      , linearLayout
          [ orientation HORIZONTAL
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , visibility if shouldShowIcon then VISIBLE else GONE
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
  searchType : state.props.currentSearchResultType
  , rating : state.data.rating
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
}

getTripDetails :: DriverInfoCardState -> TripDetails Action
getTripDetails state = {
  rideStarted : state.props.currentStage == RideStarted
  , source : state.data.source
  , destination : state.data.destination
  , onAnimationEnd : NoAction
  , backgroundColor : Color.white900
  , enablePaddingBottom : true
}

driverPickUpStatusText :: DriverInfoCardState -> String -> String
driverPickUpStatusText state _ = 
  case state.props.zoneType of
    SPECIAL_PICKUP -> getString DRIVER_AT_PICKUP_LOCATION
    _ -> if state.data.waitingTime == "--" then getString DRIVER_IS_ON_THE_WAY else getString DRIVER_IS_WAITING_AT_PICKUP 


rideNotStarted :: DriverInfoCardState -> Boolean
rideNotStarted state = 
  let lastStage = if state.props.isChatWithEMEnabled then RideStarted else RideAccepted
  in Array.any (_ == state.props.currentStage) [RideAccepted, ChatWithDriver] && lastStage == RideAccepted