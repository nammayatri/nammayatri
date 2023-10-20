{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RentalScheduleRide.View where

import Common.Types.App

import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, Direction(..))
import Common.Types.App (LazyCheck(..))
import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.PrimaryButton as PrimaryButton
import Components.ChooseVehicle.View as ChooseVehicle
import Components.RentalScheduleRide.Controller (Action(..), RentalScheduleState(..))
import Data.Array (mapWithIndex, length)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth, setText)
import Engineering.Helpers.LogEvent (logEvent)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, getPreviousVersion, getSearchType)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import JBridge (getBtnLoader, showKeyboard, getCurrentPosition, firebaseLogEvent, debounceFunction)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not, discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Accessiblity(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), accessibilityHint ,adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, clickable, color, cornerRadius, cursorColor, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, inputTypeI, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility, fontSize)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants (getDelayForAutoComplete)
import Screens.Types (RentalStage(..), SearchLocationModelType(..), LocationListItemState)
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Data.String as DS
import Helpers.Utils as HU
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes 

view :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ][ case state.isCancelled of
        true -> rideScheduledView push state
        false -> cancelledRideView push state
    ]

rideScheduledView :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
rideScheduledView push state = 
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push (const $ GoBack)
    ][ linearLayout
        [ height $ V 48
        , width $ V 48
        , onClick push (const GoBack)
        , disableClickFeedback true
        , margin (Margin 16 10 16 0)
        , gravity CENTER
        -- , orientation VERTICAL
        ]
        [ imageView
            [ height $ V 24
            , width $ V 24
            , accessibilityHint "Back : Button"
            , accessibility ENABLE
            , imageWithFallback $ "ny_ic_chevron_left" 
            ]
        ]
         , rideScheduled push state
         , notification push state
         , primaryButtonView push state
    ]

primaryButtonConfig :: RentalScheduleState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text =  state.primaryButton.text
        , color = Color.yellow900
        , height = V 40
        }
      , height = V state.primaryButton.height
      , gravity = CENTER
      , cornerRadius = state.primaryButton.cornerRadius
      , background = state.primaryButton.background
      , margin = (MarginHorizontal 16 16)
      , isClickable = true
      , id = "RentalConfirmBooking"
      }
  in primaryButtonConfig'

secondaryButtonConfig :: RentalScheduleState -> PrimaryButton.Config
secondaryButtonConfig state = 
  let
    config = PrimaryButton.config
    secondaryButtonConfig' = config
      { textConfig
        { text = "Home"
        , color = Color.yellow900
        , height = V 40
        }
      , height = V state.primaryButton.height
      , gravity = CENTER
      , cornerRadius = state.primaryButton.cornerRadius
      , background = Color.white900
      , margin = (MarginHorizontal 16 16)
      , isClickable = true
      , id = "RentalConfirmBooking"
      }
  in secondaryButtonConfig'

primaryButtonView :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
primaryButtonView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 0.0
    , gravity BOTTOM
    , margin (MarginBottom 14)
    ][  PrimaryButton.view (push <<< PrimaryButtonActionController)(primaryButtonConfig state)
      , if(state.isCancelled == true) then PrimaryButton.view (push <<< PrimaryButtonActionController) (secondaryButtonConfig state) else textView []
    ]

rideScheduled :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
rideScheduled push state =
  linearLayout
   [height WRAP_CONTENT
    , width MATCH_PARENT
    , weight 0.0
    , orientation VERTICAL]
    [ imageView
      [ width MATCH_PARENT
       ,height $ V 180 
       ,imageWithFallback $ "ys_ic_ride_scheduled," <>  ( HU.getAssetStoreLink FunctionCall) <> "ys_ic_ride_scheduled.png"
      ],
      textView $
      [ width MATCH_PARENT
       ,height WRAP_CONTENT
       ,text "Ride Scheduled"
       ,color Color.black800
       ,margin $ MarginTop 16
       ,gravity CENTER
      ] <> FontStyle.h1 TypoGraphy
      ,
      rideRescheduledDetials push state
    ]

rideRescheduledDetials :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
rideRescheduledDetials push state =
  linearLayout[
    width MATCH_PARENT
   ,height WRAP_CONTENT
   ,cornerRadii $ Corners 8.0 true true true true
   ,stroke $ "1," <> Color.grey900
   ,margin $ Margin 16 32 16 0
   ,padding $ Padding 16 16 16 16
   ,orientation VERTICAL
  ][
    rideStartDetails push state
    ,
    linearLayout[
      width MATCH_PARENT
     ,height $  V 1
     ,margin $ MarginTop 16
     ,background Color.grey900
    ][]
    ,
    sourceDetailsView push state
    ,
    linearLayout
    [
      width MATCH_PARENT
     ,height WRAP_CONTENT
     ,orientation HORIZONTAL
     ,weight 1.0
     ,margin $ MarginTop 16
    ]
    [ textView (
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , accessibility ENABLE
              , textSize FontSize.a_12
              , text $ "Rental Package"
              , margin $ MarginRight 8
              , color Color.black700
              ] <> FontStyle.body1 TypoGraphy) 
      ,
      textView (
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , accessibility ENABLE
              , textSize FontSize.a_14
              , text $ "8hr"
              , color Color.black800
              ] <> FontStyle.body1 TypoGraphy) 
            , imageView 
                [ imageWithFallback $ "ys_ic_elipse," <> (getAssetStoreLink FunctionCall) <> "ys_ic_elipse.png"
                , height $ V 5
                , width $ V 5
                , gravity CENTER
                , margin $ MarginHorizontal 6 6
                ]
            , textView(
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , accessibility ENABLE
              , textSize FontSize.a_14
              , text "80km"
              , color Color.black800
              ] <> FontStyle.subHeading1 TypoGraphy)
          ]
  ]

rideStartDetails :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
rideStartDetails push state =
   linearLayout[
      width MATCH_PARENT
     ,height WRAP_CONTENT
     ,orientation HORIZONTAL
     ,weight 0.0
    ][
      linearLayout[
        height WRAP_CONTENT
       ,width WRAP_CONTENT
       ,orientation VERTICAL
      ]
      [
        textView $ [
          width WRAP_CONTENT
         ,height WRAP_CONTENT
         ,color Color.black700
         ,text "Ride starts on"
         ,textSize FontSize.a_14
        ] <>  FontStyle.body3 TypoGraphy
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , background Color.white900
          , gravity CENTER
          , weight 1.0
          , orientation HORIZONTAL
          ] [ textView (
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , accessibility ENABLE
              , textSize FontSize.a_14
              , text $ "Fri, 10 Oct "
              , color Color.black700
              , margin $ MarginRight 8
              ] <> FontStyle.body1 TypoGraphy) 
            , imageView 
                [ imageWithFallback $ "ys_ic_elipse," <> (getAssetStoreLink FunctionCall) <> "ys_ic_elipse.png"
                , height $ V 5
                , width $ V 5
                , margin $ MarginHorizontal 6 6
                ]
            , textView(
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , accessibility ENABLE
              , textSize FontSize.a_14
              , text "6:55pm"
              , color Color.black800
              ] <> FontStyle.subHeading1 TypoGraphy)
          ]
      ]
      ,
      textView $ [
        width WRAP_CONTENT
       ,height WRAP_CONTENT
       ,text "₹420"
       ,weight 0.1
       ,gravity RIGHT
       ,textSize FontSize.a_18
      ] <> FontStyle.h2 TypoGraphy


    ]

sourceDetailsView :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
sourceDetailsView push state =
  linearLayout[
    width MATCH_PARENT
  ,height WRAP_CONTENT
  ,orientation HORIZONTAL
  ,margin $ MarginTop 16
  ]
  [
    imageView
    [ height $ V 14
    , width $ V 14
    , margin $ MarginTop 4
    , imageWithFallback $ "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_source_dot.png"
    ]
    ,
    textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ MarginLeft 8
        , text "Juspay Technologies, Stallion Business Center, 444, 18th Main Rd, 6th Block, Koramangala, Bengaluru, Karnataka 560095"
        , color Color.black900
        , margin (MarginBottom 25)
        , textSize FontSize.a_14
        , color Color.black900
        , ellipsize true
        , maxLines 2
        ] <> FontStyle.body1 TypoGraphy
  ]


notification :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
notification push state  =
  linearLayout[
    width MATCH_PARENT
   ,height WRAP_CONTENT
   ,orientation HORIZONTAL
   ,background  Color.blue600
   ,margin $ Margin 16 16 16 16
   ,padding $ Padding 16 16 16 16
   ,cornerRadii $ Corners 8.0 true true true true
  ]
  [
    imageView 
    [ imageWithFallback $ "ys_ic_alarm," <> (getAssetStoreLink FunctionCall) <> "ys_ic_alarm.png"
    , height $ V 14
    , width $ V 12
    , margin $ MarginHorizontal 6 6
    ]
    ,
    textView $[
      height WRAP_CONTENT
     ,width WRAP_CONTENT
     ,margin $ MarginLeft 8
     ,text "Driver will be assigned 15 minutes before starting the ride"
     ,color Color.black800
     ,textSize FontSize.a_12
   ] <> FontStyle.body3 TypoGraphy
  ]

cancelledRideView :: forall w. (Action -> Effect Unit) -> RentalScheduleState -> PrestoDOM (Effect Unit) w
cancelledRideView push state =
  linearLayout 
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , weight 0.1
  ]
  [ imageView [
      imageWithFallback $ "ys_ic_ride_cancelled," <> (getAssetStoreLink FunctionCall) <> "ys_ic_ride_cancelled.png"
    , width MATCH_PARENT
    , margin $ Margin 16 ((screenHeight unit) / 4) 16 0
    , height $ V 200
    ]
  , textView $ [
      text "No driver available"
    , margin $ Margin 24 26 24 12
    , gravity CENTER_HORIZONTAL
    ] <> FontStyle.h2 TypoGraphy
  , textView $ [
      text "Sorry, we couldn’t find any driver for your rental ride due to high demand.  You may try looking for rides again"
    , margin $ Margin 24 0 24 0 
    , gravity CENTER_HORIZONTAL
    -- , height $ V 168
    ]
  , primaryButtonView push state
]
