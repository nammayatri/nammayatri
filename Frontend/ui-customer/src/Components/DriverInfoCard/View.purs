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

import Animation (fadeIn, fadeInWithDelay, scaleYAnimWithDelay, fadeInWithDuration)
import Common.Styles.Colors as CommonColor
import Common.Types.App (LazyCheck(..))
import Components.DriverInfoCard.Controller (Action(..), DriverInfoCardState)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Function.Uncurried (runFn1)
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, length, take, drop, replaceAll, Replacement(..), contains, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, getExpiryTime, getNewIDWithTag, os, safeMarginBottom, screenWidth)
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Engineering.Helpers.Utils (showAndHideLoader)
import Engineering.Helpers.Utils (showAndHideLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getAssetsBaseUrl, getCommonAssetStoreLink, getPaymentMethod, secondsToHms, zoneOtpExpiryTimer, makeNumber, getVariantRideType)
import JBridge (fromMetersToKm)
import JBridge (fromMetersToKm, getArray, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (<), (-), (*), bind, pure, discard, not, (&&), (||), (/=))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Accessiblity(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, alignParentLeft, alignParentRight, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gradient, gravity, height, horizontalScrollView, id, imageUrl, imageView, imageWithFallback, layoutGravity, letterSpacing, lineHeight, linearLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollBarX, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (Stage(..), ZoneType(..), SearchResultType(..))
import Storage (KeyStore(..))
import Storage (isLocalStageOn, getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)

view :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  linearLayout
  [ height WRAP_CONTENT
  , width $ V (screenWidth unit)
  , background Color.transparent
  , orientation VERTICAL
  , afterRender push $ const $ NoAction
  ][ 
    driverInfoViewSpecialZone push state
    , driverInfoView push state
    ]

driverInfoViewSpecialZone :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoViewSpecialZone push state =
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
          , padding $ PaddingBottom 30
          , margin $ MarginTop 14
          , background Color.grey700
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true false false
          , stroke $ "1," <> Color.grey900
          ][ linearLayout
              [ gravity CENTER
              , background Color.transparentGrey
              , height $ V 4
              , width $ V 34
              , margin (MarginVertical 8 12)
              , cornerRadius 4.0
              ][]
            , titleAndETA push state
            , separator (MarginHorizontal 16 16) (V 1) Color.grey900 (state.props.currentStage == RideStarted && (secondsToHms state.data.eta) /= "" && (state.props.currentSearchResultType == QUOTES && (state.props.estimatedTime /= "--")))
            , driverDetailsView push state "SpecialDriverDetailsView"
            , separator (MarginHorizontal 16 16) (V 1) Color.grey900 true
            , paymentMethodView push state (getString PAY_VIA_CASH_OR_UPI <> " :-") false "SpecialPaymentMethodView"
            , separator (MarginHorizontal 16 16) (V 1) Color.grey900 true
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][ dropPointView push state
                , separator (MarginHorizontal 16 16) (V 1) Color.grey900 (state.props.currentStage == RideAccepted)
                , cancelRideLayout push state
              ]
            ]
      ]
  ]

titleAndETA :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
titleAndETA push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , afterRender push $ const $ NoAction
  , padding $ Padding 16 16 16 16
  , visibility $ if ((state.props.currentStage /= RideAccepted && (secondsToHms state.data.eta) == "") || (state.props.currentStage == RideStarted && (state.props.estimatedTime == "--"))) then GONE else VISIBLE
  ][ if state.props.currentStage == RideAccepted then specialZoneHeader (getValueToLocalStore SELECTED_VARIANT)
      else
      textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ "ETA: " <> if state.props.currentSearchResultType == QUOTES then (state.props.estimatedTime) else (secondsToHms state.data.eta)
      , color Color.black800
      ] <> FontStyle.h2 TypoGraphy
  ]

specialZoneHeader :: forall w. String -> PrestoDOM ( Effect Unit) w
specialZoneHeader vehicleVariant =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
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
          , visibility if (getValueToLocalStore LANGUAGE_KEY == "ML_IN") then GONE else VISIBLE
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
          , visibility if (getValueToLocalStore LANGUAGE_KEY == "ML_IN") then VISIBLE else GONE
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ getString TAXI_FROM_ZONE
          , color Color.black800
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy]

  ]

getTitleConfig :: forall w. String -> {text :: String , color :: String}
getTitleConfig vehicleVariant =
  (case vehicleVariant of
        "TAXI_PLUS" -> { text : (getString AC) <> " " <> (getString TAXI), color : Color.blue800 }
        "TAXI" -> {text : (getString NON_AC )<> " " <> (getString TAXI) , color : CommonColor.orange900 }
        _ -> {text : "" , color : ""})


dropPointView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
dropPointView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 16 0 16 16
  , background Color.white900
  , cornerRadius 8.0
  , padding $ Padding 0 10 16 if os == "IOS" then if safeMarginBottom == 0 then 16 else safeMarginBottom else 16
  ][  textView $
      [ text $ getString DROP <> " :-"
      , margin $ Margin 16 0 0 5
      ] <> FontStyle.body3 TypoGraphy
    , textView $
      [ text state.data.destination
      , color Color.black800
      , margin $ Margin 16 0 0 5
      ] <> FontStyle.subHeading1 TypoGraphy
    , estimatedTimeAndDistanceView push state
  ]

estimatedTimeAndDistanceView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
estimatedTimeAndDistanceView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity CENTER
  , afterRender push $ const $ NoAction
  , margin $ Margin 16 4 16 0
  ][ textView (
      [ text $ state.data.estimatedDistance <> "km " <> getString AWAY
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black650
      , height WRAP_CONTENT
      ] <> FontStyle.paragraphText TypoGraphy)
    -- , linearLayout
    --   [height $ V 4
    --   , width $ V 4
    --   , cornerRadius 2.5
    --   , background Color.black600
    --   , margin (Margin 6 2 6 0)
    --   ][]
    -- , textView
    --   [ text state.data.estimatedDropTime
    --   , textSize FontSize.a_14
    --   , width MATCH_PARENT
    --   , gravity CENTER
    --   , color Color.black650
    --   , height WRAP_CONTENT
    --   , fontStyle $ FontStyle.regular LanguageStyle
    --   ]
  ]

otpView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
otpView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 16.0
  , margin $ Margin 0 4 4 4
  , background Color.grey700
  ][ textView $ 
     [ text $ state.data.otp
     , color Color.black900
     , padding $ Padding 8 8 8 8
     ] <> FontStyle.body15 TypoGraphy
  ]

expiryTimeView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
expiryTimeView push state =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  , margin $ Margin 16 0 16 16
  , cornerRadius 9.0
  , background Color.grey800
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ][ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , padding $ Padding 10 14 10 14
          , weight 1.0
          ][ textView (
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , accessibilityHint $ "O.T.P is :" <> (replaceAll (Pattern "") (Replacement " ")  state.data.otp)
              , accessibility ENABLE
              , text $ getString OTP <> ":"
              , color Color.black700
              ] <> FontStyle.body4 TypoGraphy)
            , otpView push state
          ]
      ]
  ]

mapOptionsView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
mapOptionsView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.transparent
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding $ PaddingHorizontal 16 16
  ][ linearLayout
      [ height WRAP_CONTENT
      , weight 1.0
      , clickable false
      ][]
      , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginVertical 5 5
      ][ if state.props.currentSearchResultType == QUOTES && state.props.currentStage == RideAccepted then navigateView push state else (textView[height $ V 0])
        , if state.props.currentSearchResultType == QUOTES && state.props.currentStage == RideAccepted then dummyView push else locationTrackButton push state
      ]
    ]

locationTrackButton :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
locationTrackButton push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , background Color.white900
  , stroke $ "1,"<> Color.grey900
  , visibility if (Array.any (_ == state.props.currentStage) [ RideAccepted, RideStarted, ChatWithDriver ]) && (not state.props.showChatNotification) && state.data.config.driverInfoConfig.showTrackingButton then VISIBLE else GONE
  , cornerRadius 20.0
  , accessibility DISABLE_DESCENDANT
  , onClick push (const $ LocationTracking)
  , margin $ MarginTop 8
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.white900
      , stroke $ "1,"<> Color.grey900
      , cornerRadius 20.0
      ][  imageView
        [ imageWithFallback $ "ny_ic_location_track," <> (getAssetStoreLink FunctionCall) <> "ny_ic_location_track.png"
        , height $ V 18
        , width $ V 18
        , margin $ Margin 10 10 10 10
        ]
      ]
  ]



navigateView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
navigateView push state =
  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.white900
      , padding $ Padding 20 16 20 16
      , margin $ MarginTop 16
      , cornerRadius 25.0
      , gravity CENTER
      , stroke $ "1,"<>Color.grey900
      , orientation HORIZONTAL
      , onClick push (const OnNavigateToZone)
      ][  imageView
          [ width $ V 20
          , height $ V 20
          , imageWithFallback $ "ny_ic_walk_mode_blue," <>  (getAssetStoreLink FunctionCall) <> "ny_ic_walk_mode_blue.png"
          ]
        , textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin (MarginLeft 8)
          , text $ getString GO_TO_ZONE
          , gravity CENTER
          , color Color.blue900
          ] <> FontStyle.body1 TypoGraphy
          )
      ]

driverInfoView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility if state.props.currentSearchResultType == QUOTES then GONE else VISIBLE
  , margin $ MarginBottom if os == "IOS" then safeMarginBottom else 0
  , afterRender push $ const $ NoAction
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
         [ orientation VERTICAL
         , height WRAP_CONTENT
         , width MATCH_PARENT
         , background if true then Color.blue800 else Color.grey700
         , gravity CENTER
         , cornerRadii $ Corners 24.0 true true false false
         , stroke $ state.data.config.driverInfoConfig.cardStroke
         ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , background Color.blue800
            , cornerRadii $ Corners 24.0 true true false false
            , gravity CENTER
            , id $ getNewIDWithTag "MetroRideView"
            , orientation HORIZONTAL
            , padding (PaddingVertical 4 4)
            , visibility if true then VISIBLE else GONE
            ][ imageView
                [ width (V 15)
                , height (V 15)
                , margin (MarginRight 6)
                , accessibility DISABLE
                , imageWithFallback $  "ny_ic_metro_white," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_metro_white.png"
                ]
              , textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , textSize FontSize.a_14
                , accessibility if true then ENABLE else DISABLE
                , accessibilityHint "Metro Ride"
                , text (getString METRO_RIDE)
                , color Color.white900
                ]
             ]
          , linearLayout
            [ orientation VERTICAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ PaddingBottom 24
            , background Color.grey700
            , gravity CENTER
            , cornerRadii $ Corners 24.0 true true false false
            , stroke $ "1," <> Color.grey900
            ][ linearLayout
              [ gravity CENTER
              , background Color.transparentGrey
              , height $ V 4
              , width $ V 34
              , accessibility ENABLE
              , accessibilityHint "Bottom Sheet : Swipe Up Or Down : To Expand Or Collapse : Button"
              , margin (MarginTop 8)
              , onClick push $ const ExpandBottomSheet
              , cornerRadius 4.0
              ][]
              , linearLayout
                [ height $ WRAP_CONTENT
                , width $ MATCH_PARENT
                , orientation VERTICAL
                , id $ "DriverInfoCardView"
                , afterRender push $ const $ NoAction
                ][ if state.props.currentSearchResultType == QUOTES  then headerTextView push state else contactView push state
                 , if state.props.currentStage == RideStarted then distanceView push state else dummyView push
                 , separator (Margin 16 (if(state.props.currentStage == RideStarted && state.data.config.nyBrandingVisibility) then 16 else 0) 16 0) (V 1) Color.grey900 $ ((state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) && state.data.config.nyBrandingVisibility) || (state.props.currentStage == RideAccepted && not state.data.config.showPickUpandDrop)
                 , driverDetailsView push state "DriverDetailsView"
                 , paymentMethodView push state (getString FARE_ESTIMATE) true "PaymentMethodView"
                ]
              , linearLayout
                [ width MATCH_PARENT
                , height if os == "IOS" then (V 210) else WRAP_CONTENT
                , orientation VERTICAL
                ][ if state.props.currentSearchResultType == QUOTES then destinationView push state else if state.data.config.showPickUpandDrop == false then dummyView push else sourceDistanceView push state
                , cancelRideLayout push state
                ]
              ]
         ]
      ]
  ]
distanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
distanceView push state = 
  linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 20 16 16
    , id $ getNewIDWithTag "DriverInfoDistanceView"
    ][
      textView $
            [ text $ getString REACHING_YOUR_DESTINATION_IN_
            , color Color.black800
            , ellipsize true
            , singleLine true
            ] <> FontStyle.subHeading1 TypoGraphy
            ,textView $
            [ text $ secondsToHms state.data.eta
            , color Color.green900
            , ellipsize true
            , singleLine true
            ] <> FontStyle.subHeading1 TypoGraphy
    ]
cancelRideLayout :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
cancelRideLayout push state =
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay state)] $ 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , afterRender push $ const $ NoAction
  , onAnimationEnd push $ const $ NoAction
  , margin $ if state.data.config.showPickUpandDrop then MarginTop 6 else MarginTop 0
  , padding $ PaddingBottom if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0
  , visibility if (Array.any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver ]) then VISIBLE else GONE
  ][ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ Padding 5 5 5 5
    , accessibilityHint "Cancel Ride : Button"
    , accessibility ENABLE
    , margin $ MarginBottom if os == "IOS" then 24 else 0
    , onClick push $ const $ CancelRide state
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
    , id $ getNewIDWithTag "DriverInfoHeaderView"
    , afterRender push $ const $ NoAction
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 20 16 16
    , visibility if (Array.any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver ]) then VISIBLE else GONE
    ][  linearLayout
        [ width (V (((screenWidth unit)/3 * 2)-27))
        , height WRAP_CONTENT
        , accessibilityHint $ "Ride Status : " <>  if state.data.distance > 1000 then (state.data.driverName <> " is " <> secondsToHms state.data.eta <> " Away ") else (state.data.driverName <> if state.data.waitingTime == "--" then " is on the way" else " is waiting for you.")
        , accessibility ENABLE
        , orientation if length state.data.driverName > 16 then VERTICAL else HORIZONTAL
        ][  textView $
            [ text $ state.data.driverName <> " "
            , color Color.black900
            , ellipsize true
            , singleLine true
            ] <> FontStyle.subHeading1 TypoGraphy
          , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ][ textView $
                [ text $"is " <> secondsToHms state.data.eta
                , color Color.black900
                , visibility if (state.data.distance > 1000 && (secondsToHms state.data.eta) /= "") then VISIBLE else GONE
                ] <> FontStyle.subHeading1 TypoGraphy
              , textView $
                [ text case (state.data.distance > 1000 && (secondsToHms state.data.eta) /= "") of
                    true -> getString AWAY
                    false -> if state.data.waitingTime == "--" then getString IS_ON_THE_WAY else getString IS_WAITING_AT_PICKUP
                , color Color.black900
                ] <> FontStyle.subHeading1 TypoGraphy
              ]
          ]
      , linearLayout[
          width MATCH_PARENT
        , gravity RIGHT
        , height WRAP_CONTENT
        ][linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity RIGHT
          ][ linearLayout
             [ height $ V 40
             , width $ V 64
             , gravity CENTER
             , cornerRadius 32.0
             , background state.data.config.driverInfoConfig.callBackground
             , stroke state.data.config.driverInfoConfig.callButtonStroke
             , onClick push $ const $ MessageDriver
             , accessibilityHint "Chat or Call : Button"
             , accessibility ENABLE
             ][ imageView
                 [ imageWithFallback $ if (getValueFromConfig "isChatEnabled") == "true" then if state.props.unReadMessages then "ic_chat_badge_green," <> (getAssetStoreLink FunctionCall) <> "ic_chat_badge_green.png" else "ic_call_msg," <> (getAssetStoreLink FunctionCall) <> "ic_call_msg.png" else "ny_ic_call," <> (getAssetStoreLink FunctionCall) <> "ny_ic_call.png"
                 , height $ V state.data.config.driverInfoConfig.callHeight
                 , width $ V state.data.config.driverInfoConfig.callWidth
                 ]
             ]
           ]
        ]
       ]


---------------------------------- driverDetailsView ---------------------------------------


driverDetailsView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> String -> PrestoDOM (Effect Unit) w
driverDetailsView push state uid =
 linearLayout
  [ orientation HORIZONTAL
  , height $ V 150
  , padding $ Padding 16 0 16 0
  , width MATCH_PARENT
  , afterRender push $ const $ NoAction
  , id $ getNewIDWithTag uid
  , margin $ Margin 16 0 16 16
  , background Color.white900
  , cornerRadius 8.0
  , visibility if state.props.currentSearchResultType == QUOTES then (if state.props.currentStage == RideStarted then VISIBLE else GONE) else VISIBLE
  , gravity BOTTOM
  ][  linearLayout
      [ orientation VERTICAL
      , height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , alignParentLeft "true,-1"
      ][linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity LEFT
        ][ frameLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , margin $ MarginBottom 12
          ][imageView
            [ height $ V 50
            , width $ V 50
            , padding $ Padding 2 3 2 1
            , imageWithFallback $ "ny_ic_driver," <> (getAssetStoreLink FunctionCall) <> "ny_ic_driver.png"
            , accessibilityHint $ "Driver : " <> state.data.driverName <> " : Vehicle Number : " <> state.data.registrationNumber
            , accessibility ENABLE
            ]
            , ratingView push state
            ]
          ]
        , textView $
          [ text state.data.driverName
          , maxLines 1
          , ellipsize true
          , accessibility DISABLE
          , color Color.black800
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity LEFT
          ] <> FontStyle.body9 TypoGraphy
        , textView (
          [ text $ state.data.vehicleDetails <> " " 
                    <> case getMerchant FunctionCall of
                          YATRISATHI -> "· " <> getVariantRideType state.data.vehicleVariant
                          _          -> case state.data.vehicleVariant of
                                          "TAXI_PLUS" -> " (" <> (getString AC_TAXI) <> ")"
                                          "TAXI" -> " (" <> (getString NON_AC_TAXI) <> ")"
                                          _ -> ""
          , color Color.black700
          , accessibility DISABLE
          , width $ V ((screenWidth unit) /2 - 20)
          , maxLines 2
          , singleLine false
          , height WRAP_CONTENT
          , gravity LEFT
          ] <> FontStyle.captions TypoGraphy)
      ]
    , linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , accessibility DISABLE_DESCENDANT
      ][  relativeLayout
          [ height MATCH_PARENT
          , width $ MATCH_PARENT
          , gravity RIGHT
          ][  imageView
              [ imageWithFallback (getVehicleImage state.data.vehicleVariant state.data.vehicleDetails)
              , height $ V 120
              , gravity RIGHT
              , width MATCH_PARENT
              , margin $ MarginBottom 25
              ]
            , linearLayout
              [ height $ WRAP_CONTENT
              , width $ MATCH_PARENT
              , gravity RIGHT
              , alignParentBottom "true,-1"
              ][ linearLayout
                [ height $ V 36
                , width WRAP_CONTENT
                , background state.data.config.driverInfoConfig.numberPlateBackground
                , cornerRadius 4.0
                , gravity RIGHT
                , orientation HORIZONTAL
                , margin $ MarginBottom 16
                , padding $ Padding 2 2 2 2
                ][linearLayout
                  [ height $ V 32
                  , width WRAP_CONTENT
                  , stroke $ "2," <> Color.black
                  , cornerRadius 4.0
                  , gravity RIGHT
                  , orientation HORIZONTAL
                  ][ imageView
                      [ imageWithFallback $ "ny_ic_number_plate," <> (getAssetStoreLink FunctionCall) <> "ny_ic_number_plate.png"
                      , gravity LEFT
                      , visibility if state.data.config.driverInfoConfig.showNumberPlatePrefix then VISIBLE else GONE
                      , background "#1C4188"
                      , height MATCH_PARENT
                      , width $ V 22
                      ]
                  ,  textView $
                    [ margin $ Margin 2 2 2 2
                    , height $ MATCH_PARENT
                    , text $ (makeNumber state.data.registrationNumber)
                    , color Color.black
                    ] <> FontStyle.body7 TypoGraphy
                  , imageView
                    [ imageWithFallback $ "ny_ic_number_plate_suffix," <> (getAssetStoreLink FunctionCall) <> "ny_ic_number_plate_suffix.png"
                    , gravity RIGHT
                    , visibility if state.data.config.driverInfoConfig.showNumberPlateSuffix then VISIBLE else GONE
                    , height MATCH_PARENT
                    , width $ V 13
                    ]
                    ]
                 ]
              ]
          ]
      ]
  ]

---------------------------------- ratingView ---------------------------------------

ratingView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
ratingView push state =
  linearLayout
  [ orientation HORIZONTAL
  , margin $ MarginTop 40
  , height $ V 19
  , width $ V 50
  , afterRender push $ const $ NoAction
  , padding $ Padding 8 3 6 3
  , background state.data.config.driverInfoConfig.ratingBackground
  , gravity CENTER_VERTICAL
  , stroke  state.data.config.driverInfoConfig.ratingStroke
  , cornerRadius state.data.config.driverInfoConfig.ratingCornerRadius
  , accessibility DISABLE
  ][  imageView
      [ imageWithFallback $ "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_active.png"
      , height $ V 9
      , width $ V 9
      , accessibility DISABLE
      ]
    , textView $
      [ text $ if state.data.rating == 0.0 then (getString NEW_) else show state.data.rating
      , color state.data.config.driverInfoConfig.ratingTextColor
      , gravity CENTER_VERTICAL
      , margin (Margin 3 0 3 1)
      , textSize $ FontSize.a_10
      , accessibility DISABLE
      ] <> FontStyle.tags TypoGraphy
    ]

---------------------------------- paymentMethodView ---------------------------------------

paymentMethodView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> String -> Boolean -> String -> PrestoDOM (Effect Unit) w
paymentMethodView push state title shouldShowIcon uid =
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , afterRender push $ const $ NoAction
  , id $ getNewIDWithTag uid
  , margin $ Margin 16 0 16 0
  , background Color.white900
  , padding $ Padding 16 16 16 16
  , cornerRadius 8.0
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , accessibility ENABLE
      , accessibilityHint $ "Fare Estimate :" <> state.data.config.currency <> show state.data.price
      ][  textView $
          [ text title
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ state.data.config.currency <> show state.data.price
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
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
              [ imageWithFallback $ "ny_ic_wallet_filled," <> (getAssetStoreLink FunctionCall) <> "ny_ic_wallet_filled.png"
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

---------------------------------- tripDetailsView ---------------------------------------

sourceDistanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
sourceDistanceView push state =
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay state)] $ 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 16 12 16 16
  , background Color.white900
  , afterRender push $ const $ NoAction
  , onAnimationEnd push $ const $ NoAction
  , cornerRadius 8.0
  , padding $ Padding 0 10 0 if (os == "IOS" && state.props.currentStage == RideStarted) then safeMarginBottom else 16
  ][linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , padding $ Padding 16 4 16 16
    , width WRAP_CONTENT
    , gravity LEFT
    , accessibility ENABLE
    , accessibilityHint $ "Pickup location is :" <> state.data.source
    ][linearLayout
      [ orientation HORIZONTAL
      , gravity CENTER
      ][imageView
        [ imageWithFallback $ "ny_ic_pickup," <> (getAssetStoreLink FunctionCall) <> "ny_ic_pickup.png"
        , height $ V 8
        , width $ V 8
        ]
        ,textView $
         [ text $ getString PICKUP
         , margin $ MarginLeft 6
         , padding $ PaddingBottom 3
         , color Color.black700
         ] <> FontStyle.body3 TypoGraphy
        ]
       , textView $
         [ text state.data.source
         , maxLines 1
         , ellipsize true
         , width $ V ((screenWidth unit) / 10 * 6)
         , height MATCH_PARENT
         , gravity LEFT
         , color Color.black900
         , margin $ MarginTop 6
         ] <> FontStyle.body1 TypoGraphy
      ]
    , separator (MarginHorizontal 16 16) (V 1) "#F3F4F8" true
    ,linearLayout
    [ orientation VERTICAL
      , height WRAP_CONTENT
      , padding $ Padding 16 6 16 4
      , width WRAP_CONTENT
      , gravity LEFT
      , accessibility ENABLE
      , accessibilityHint $ "Drop location is :" <> state.data.destination
    ][linearLayout
      [ orientation HORIZONTAL
      , gravity CENTER
      ][imageView
        [ imageWithFallback $ "ny_ic_drop," <> (getAssetStoreLink FunctionCall) <> "ny_ic_drop.png"
        , height $ V 8
        , width $ V 8
        ]
      , textView $ 
        [ text $ getString DROP
        , margin $ MarginLeft 6
        , padding $ PaddingBottom 3
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
      ]
      , textView $
        [ text state.data.destination
        , maxLines 1
        , ellipsize true
        , width $ V ((screenWidth unit) / 10 * 6)
        , height MATCH_PARENT
        , gravity LEFT
        , margin $ MarginTop 6
        , color Color.black900
        ] <> FontStyle.body1 TypoGraphy
     ]
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
        , imageUrl = "ny_ic_call," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_call.png"
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
        imageUrl = "ny_ic_pickup," <> (getAssetStoreLink FunctionCall) <> "ny_ic_pickup.png"
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
        imageUrl = "ny_ic_drop," <> (getAssetStoreLink FunctionCall) <> "ny_ic_drop.png"
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

headerTextView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
headerTextView push state =
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , padding $ Padding 16 20 16 16
  ][ if state.props.currentStage == RideStarted then
      textView $
      [ text $ "ETA :" <> state.props.estimatedTime
      , color Color.black800
      , padding $ PaddingBottom 16
      , ellipsize true
      ] <> FontStyle.body8 TypoGraphy
      else specialZoneHeader (getValueToLocalStore SELECTED_VARIANT)
    ,  separator (MarginHorizontal 16 16) (V 1) Color.grey900 (state.props.currentStage == RideStarted)
    , if state.props.currentStage == RideStarted then  contactView push state else linearLayout[][]
  ]

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
      , onClick push (const OnNavigate)
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
          , imageWithFallback $ "ny_ic_navigation," <> (getCommonAssetStoreLink FunctionCall) <> "driver/images/ny_ic_navigation.png"
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

getVehicleImage :: String -> String -> String
getVehicleImage variant vehicleDetail = do
  let url = getAssetStoreLink FunctionCall
  let details = (toLower vehicleDetail)
  if (variant == "AUTO_RICKSHAW") then "ic_auto_rickshaw," <> url <>"ic_auto_rickshaw.png"
  else
    if contains (Pattern "ambassador") details then "ic_yellow_ambassador," <> url <> "ic_yellow_ambassador.png"
    else 
      case (getMerchant FunctionCall) of
        YATRISATHI -> case variant of
                        "SUV" -> "ny_ic_suv_concept," <> url <> "ny_ic_suv_concept.png"
                        _     -> "ny_ic_sedan_concept," <> url <> "ny_ic_sedan_concept.png"
        _          -> "ic_white_taxi," <> url <> "ic_white_taxi.png"

getAnimationDelay :: DriverInfoCardState -> Int
getAnimationDelay state = 100