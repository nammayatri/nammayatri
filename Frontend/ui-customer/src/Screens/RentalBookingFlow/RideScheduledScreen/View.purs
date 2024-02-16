module Screens.RentalBookingFlow.RideScheduledScreen.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.GenericHeader.View as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination.View as SourceToDestinationView
import Data.Array (singleton)
import Data.Maybe (maybe)
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import Screens.RentalBookingFlow.RideScheduledScreen.ComponentConfig (primaryButtonConfig, sourceToDestinationConfig, genericHeaderConfig)
import Screens.RentalBookingFlow.RideScheduledScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (RideScheduledScreenState)
import Styles.Colors as Color

rideScheduledScreen :: RideScheduledScreenState -> Screen Action RideScheduledScreenState ScreenOutput
rideScheduledScreen initialState =
  { initialState
  , view
  , name: "RideScheduledScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let _ = spy "RideScheduledScreen action " action
        let _ = spy "RideScheduledScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , separatorView push state
    , linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginTop 60
      , gravity CENTER_HORIZONTAL
      ]
      [ scheduledDetailsView push state
      , notificationView push state
      , cancelBookingView push state
      , primaryButtonView push state
      ]
    ]

scheduledDetailsView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
scheduledDetailsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , weight 0.0
  , orientation VERTICAL
  ]
  [ imageView
      [ width MATCH_PARENT
      , height $ V 180
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_ride_scheduled"
      ]
  , textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getString RIDE_SCHEDULED
      , color Color.black800
      , margin $ MarginTop 16
      , gravity CENTER
      ] <> FontStyle.h1 TypoGraphy
  , rideDetailsView push state
  ]

primaryButtonView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
primaryButtonView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 0.0
    , gravity BOTTOM
    , margin (MarginBottom 24)
    ]
    [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
    ]

rideDetailsView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
rideDetailsView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , cornerRadius 8.0
    , stroke $ "1," <> Color.grey900
    , margin $ Margin 16 32 16 0
    , padding $ Padding 16 16 16 16
    , orientation VERTICAL
    ]
    [ rideStartDetails push state
    , separatorView push state
    , sourceDestinationView push state
    , maybe (dummyView) (\_ -> addStopView push state) state.destination
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ MarginTop 7
        ]
        [ textView $
            [ textSize FontSize.a_12
            , text $ getString RENTAL_PACKAGE
            , margin $ MarginRight 8
            , color Color.black700
            ] <> FontStyle.body1 TypoGraphy
        , textView $
            [ textSize FontSize.a_14
            , text $ state.baseDuration <> "hr"
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
        , textView $
            [ textSize FontSize.a_14
            , text $ " · "
            , color Color.black800
            ] <> FontStyle.h1 TypoGraphy
        , textView $
            [ textSize FontSize.a_14
            , text $ state.baseDistance <> "km"
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
        ]
    ]
    where
      dummyView :: forall w. PrestoDOM (Effect Unit) w
      dummyView = linearLayout [height $ V 0, width $ V 0, visibility GONE] []

      addStopView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
      addStopView push state = 
        textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginLeft 28
        , color Color.blue800
        , onClick push $ const $ AddFirstStop state.destination
        , text $ getString EDIT
        ] <> FontStyle.paragraphText TypoGraphy

rideStartDetails :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
rideStartDetails push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin $ MarginBottom 11
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.black700
            , text $ getString RIDE_STARTS_ON
            , textSize FontSize.a_14
            ] <> FontStyle.body3 TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView
                ( [ textSize FontSize.a_14
                  , text $ state.startDate
                  , color Color.black700
                  ] <> FontStyle.body1 TypoGraphy
                )
            , textView $
                [ text $ " · "
                , color Color.black800
                ] <> FontStyle.body1 TypoGraphy
            , textView $
                [ textSize FontSize.a_14
                , text $ " " <> state.startTime
                , color Color.black700
                ] <> FontStyle.body1 TypoGraphy
            ]
        ]
    , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ "₹" <> state.finalPrice
        , weight 0.1
        , gravity RIGHT
        , textSize FontSize.a_18
        ] <> FontStyle.h2 TypoGraphy

    ]

notificationView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
notificationView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , background Color.blue600
    , margin $ Margin 16 16 16 16
    , padding $ Padding 16 16 16 16
    , cornerRadius 8.0
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_alarm"
        , height $ V 14
        , width $ V 12
        , margin $ MarginHorizontal 6 6
        ]
    , textView $
        [ margin $ MarginLeft 8
        , width WRAP_CONTENT
        , text $ getVarString DRIVER_WILL_BE_ASSIGNED_MINUTES_BEFORE_STARTING_THE_RIDE (singleton state.driverAllocationTime)  --  getString DRIVER_WILL_BE_ASSIGNED <> " " <> state.driverAllocationTime <> " " <> getString MINUTES_BEFORE_STARTING_THE_RIDE
        , color Color.black800
        ] <> FontStyle.body3 TypoGraphy
    ]

separatorView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , margin $ MarginVertical 5 5
    , background Color.grey900
    ]
    []

sourceDestinationView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
sourceDestinationView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginTop 11
  ]
  [ SourceToDestinationView.view (push <<< SourceToDestinationAC) (sourceToDestinationConfig state) ]

cancelBookingView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
cancelBookingView push state =
  textView $
  [ width MATCH_PARENT
  , textFromHtml $ "<u>" <> getString CANCEL_RENTAL_BOOKING <> "</u>"
  , color Color.black700
  , onClick push $ const CancelRide
  , gravity CENTER_HORIZONTAL
  ] <> FontStyle.body3 TypoGraphy