module Components.RideScheduler.View where

import Prelude
import Common.Types.App
import Effect (Effect)
import Engineering.Helpers.Commons (os)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<>), (<<<), (==), const, not, show)
import PrestoDOM 
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Data.Maybe (Maybe(..), isJust)
import Components.RideScheduler.Controller
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag, safeMarginTop, getCurrentUTC,convertDateTimeConfigToUTC,getUTCAfterNSeconds)
import Animation as Anim
import Debug(spy)


view :: forall w . (Action  -> Effect Unit) ->Config -> PrestoDOM (Effect Unit) w
view push config= 
  Anim.screenAnimation $
    linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      ]
      ([headerLayout config push
      , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.grey900
          ][]
      , scrollView
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , background Color.grey700
          , weight 1.0
          ]
          [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [
                dateAndTimePickerView config push
            ]
          ]
      , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.grey900
          ][]
      ,  buttonLayout config push
      ])
headerLayout :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerLayout config push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , background Color.white900
    , padding if os == "IOS" then (PaddingTop safeMarginTop) else (PaddingTop 0)
    ][headerLeft config push]


headerLeft :: Config -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLeft config push =
  linearLayout
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ Padding 5 16 5 16
    , weight 1.0
    ]
    [ imageView
      [ width $ V 30
      , height $ V 30
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin $ Margin 5 0 0 0
      , rippleColor Color.rippleShade
      , onClick push $ const $ ExitScheduler
      ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Schedule a Ride"
        -- , textSize FontSize.a_18
        , margin $ Margin 5 0 0 3
        , weight 1.0
        , color Color.black800
        ] <> FontStyle.h3 TypoGraphy
    ]


rideSchedulerView :: forall w. Config -> (Action  -> Effect Unit) ->  PrestoDOM (Effect Unit) w
rideSchedulerView config push = 
    linearLayout[
        height $ V 1,
        width $ V 1
    ][]

buttonLayout :: Config -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
buttonLayout config push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 16 16 16 16
    , gravity CENTER
    ]
    [ 
      acceptButton config push 
    ]


acceptButton :: Config -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
acceptButton config push =
      linearLayout 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 16 16 16 16
        , cornerRadius 8.0
        , gravity CENTER
        , background Color.black900
        , onClick push $ const $ DateSelected
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "Continue"
            , color Color.yellow900
            ] <> FontStyle.subHeading1 TypoGraphy
        ]

dateAndTimePickerView :: Config -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dateAndTimePickerView config push = 
    linearLayout
    [
        width MATCH_PARENT,
        height WRAP_CONTENT,
        cornerRadius 8.0,
        orientation VERTICAL,
        background Color.white900,
        margin $ Margin 16 16 16 16
    ][ 
        headingView config push,
        horizontalSeperatorView,
        datePickerView config push,
        horizontalSeperatorView,
        timePickerView config push,
        horizontalSeperatorView,
        infoView config push
    ]

horizontalSeperatorView :: forall w. PrestoDOM (Effect Unit) w
horizontalSeperatorView = 
        linearLayout[
            width MATCH_PARENT,
            height $ V 1,
            background $ Color.grey800,
            margin $ MarginHorizontal 16 16
        ][]

headingView :: Config -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headingView config push = linearLayout
    [
        width MATCH_PARENT,
        height WRAP_CONTENT,
        orientation VERTICAL,
        padding $ Padding 16 16 16 16
    ][
        textView $ [
            width MATCH_PARENT,
            height WRAP_CONTENT,
            text "Pickup Date and Time",
            gravity $ CENTER,
            color $ Color.black900
        ]<> FontStyle.h3 TypoGraphy
    ]


datePickerView :: Config -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
datePickerView  config push = 
    let 
        startTimeUTC = unsafePerformEffect $ convertDateTimeConfigToUTC config.startDate.year config.startDate.month config.startDate.day config.startTime.hour config.startTime.minute 0
        _ = spy "startTimeUTC inside Date" startTimeUTC
    in 
    linearLayout [
        width MATCH_PARENT,
        height WRAP_CONTENT,
        orientation VERTICAL,
        onClick push $ const $ OpenDatePicker,
        padding $ Padding 16 16 16 16
    ][
        textView $ [
            width MATCH_PARENT,
            height WRAP_CONTENT,
            text $ convertUTCtoISC startTimeUTC "ddd, D MMM, YYYY",
            gravity $ CENTER,
            color $ Color.black800
        ]<> FontStyle.subHeading1 TypoGraphy
    ]
timePickerView :: Config -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
timePickerView config push = 
    let 
        startTimeUTC =  unsafePerformEffect $ convertDateTimeConfigToUTC config.startDate.year config.startDate.month config.startDate.day config.startTime.hour  config.startTime.minute 0
        -- startTimeIST = getUTCAfterNSeconds startTimeUTC 19860
        _ = spy "startTimeUTC in Time" startTimeUTC
        _ = spy "time year " config.startDate.year 
        _ = spy "time month " config.startDate.month
        _ = spy "time day "  config.startDate.day
        _ = spy "time hour " config.startTime.hour
        _ = spy "time minute "  config.startTime.minute
        _ = spy "time seconds " 0
    in 
    linearLayout
    [
        width MATCH_PARENT,
        height WRAP_CONTENT,
        orientation VERTICAL,
        onClick push $ const $ OpenTimePicker,
        padding $ Padding 16 16 16 16
    ][
        textView $ [
            width MATCH_PARENT,
            height WRAP_CONTENT,
            text $ convertUTCtoISC startTimeUTC "hh:mm A",
            gravity $ CENTER,
            color $ Color.black800
        ]<> FontStyle.subHeading1 TypoGraphy
    ]
infoView :: Config -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
infoView config push = linearLayout
    [
        width MATCH_PARENT,
        height WRAP_CONTENT,
        padding $ Padding 16 16 16 16
    ][
         imageView
          [ width $ V 12
          , height $ V 12
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
          , margin $ MarginRight 8
          , color Color.black800
          ],
        textView $
        [
           height WRAP_CONTENT
        ,  width MATCH_PARENT
        ,  orientation VERTICAL
        ,  text "Choose the exact pickup time and date , you'll notified when the driver accepts your ride"
        ,  gravity $ CENTER
        , color $ Color.black800
        ]<> FontStyle.body3 TypoGraphy
    ]