module Screens.RentalBookingFlow.RideScheduledScreen.View where

import Prelude

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader.View as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination.View as SourceToDestinationView
import Components.PopUpModal as PopUpModal
import Data.Array (singleton, head ,filter ,any)
import Data.Either (Either(..))
import Data.Maybe (maybe, fromMaybe, Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, decodeError, storeCallBackCustomer)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..) ,background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..))
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import Screens.RentalBookingFlow.RideScheduledScreen.ComponentConfig (primaryButtonConfig, sourceToDestinationConfig, genericHeaderConfig, cancelScheduledRideConfig)
import Screens.RentalBookingFlow.RideScheduledScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.Types (RideScheduledScreenState)
import Services.API
import Services.Backend as Remote
import Styles.Colors as Color
import Helpers.CommonView (dummyView)
import Types.App (GlobalState, defaultGlobalState)
import Mobility.Prelude (boolToVisibility)
import Screens.Types (FareProductType(..)) as FPT
import Resources.LocalizableV2.Strings (getEN)
import Control.Monad.Except.Trans (lift)
import Accessor(_status,_isScheduled)
import Data.Lens((^.))

rideScheduledScreen :: RideScheduledScreenState -> Screen Action RideScheduledScreenState ScreenOutput
rideScheduledScreen initialState =
  { initialState
  , view
  , name: "RideScheduledScreen"
  , globalEvents: [getBookingListEvent]
  , eval:
      \action state -> do
        let _ = spy "RideScheduledScreen action " action
        let _ = spy "RideScheduledScreen state " state
        eval action state
  }
  where
    getBookingListEvent push = do
      void $ storeCallBackCustomer push NotificationListener "RideScheduledScreen" Just Nothing
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ getBooking GetBooking CheckFlowStatusAction 10 1000.0 push initialState
      pure $ pure unit
    getBooking :: forall action. (RideBookingRes -> action) -> action -> Int -> Number -> (action -> Effect Unit) -> RideScheduledScreenState -> Flow GlobalState Unit
    getBooking action flowStatusAction count duration push state = do
        let
          bookingId = state.data.bookingId
        if(count > 0 && bookingId /= "") then do
          rideBookingResponse <- Remote.rideBooking bookingId
          case rideBookingResponse of
            Right response -> do
              let (RideBookingRes resp) = response
              if not (resp.id == "") then doAff do liftEffect $ push $ action response
              else do
                if count == 1 then do
                  doAff do liftEffect $ push $ action response
                else do
                  void $ delay $ Milliseconds duration
                  getBooking action flowStatusAction (count - 1) duration push state
            Left err -> do
              let errResp = err.response
                  codeMessage = decodeError errResp.errorMessage "errorMessage"
              void $ delay $ Milliseconds duration
              if(count == 1) then do
                let response = dummyRideBooking
                doAff do liftEffect $ push $ action response
              else getBooking action flowStatusAction (count - 1) duration push state
        else pure unit

view :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push $ const GoBack
    , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
    ]
    ([ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , separatorView push state ]
    , linearLayout
      [ height MATCH_PARENT
      , margin $ MarginTop 60
      , width MATCH_PARENT
      ][  scrollView
          [ height MATCH_PARENT
          , margin $ MarginBottom 100
          , width MATCH_PARENT
          ][  linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , gravity CENTER_HORIZONTAL
              ] ( if state.data.bookingId == "" then
                    [shimmerFrameLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , orientation VERTICAL
                      , background Color.transparent
                      ][ sheduledDetailsShimmerView state]
                      ]
                    else
                      [ scheduledDetailsView push state
                      , notificationView push state
                      , cancelBookingView push state
                      ]
                  )
            ]
      ]
    , primaryButtonView push state
    ] <> if state.props.isCancelRide then [cancelRidePopUpView push state] else [dummyView])


sheduledDetailsShimmerView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  linearLayout[height $ V 180, width MATCH_PARENT, background Color.greyDark, margin $ Margin 40 20 40 40, cornerRadius 20.0][]
    , linearLayout[height $ V 100, width MATCH_PARENT, background Color.greyDark, margin $ Margin 16 0 16 16][]
    , linearLayout[height $ V 100, width MATCH_PARENT, background Color.greyDark, margin $ Margin 16 0 16 0 ][]
    ]

scheduledDetailsView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
scheduledDetailsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
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
      , accessibility ENABLE
      , accessibilityHint $ getEN RIDE_SCHEDULED
      ] <> FontStyle.h1 TypoGraphy
  , rideDetailsView push state
  ]

primaryButtonView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
primaryButtonView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity BOTTOM
  , margin $ MarginBottom $ if EHC.os == "IOS" then EHC.safeMarginBottom else 24
    ][ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]

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
    , maybe (dummyView) (\_ -> addStopView push state) state.data.destination
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ MarginTop 7
        , gravity CENTER_VERTICAL
        , visibility $ boolToVisibility $ state.data.fareProductType == FPT.RENTAL
        ]
        [ textView $
            [ textSize FontSize.a_12
            , text $ getString RENTAL_PACKAGE
            , margin $ MarginRight 8
            , color Color.black700
            , accessibility ENABLE
            , accessibilityHint $ getEN RENTAL_PACKAGE
            ] <> FontStyle.body1 TypoGraphy
        , textView $
            [ textSize FontSize.a_14
            , text $ state.data.baseDuration <> " hr"
            , color Color.black800
            , accessibility ENABLE
            , accessibilityHint $ state.data.baseDuration <> " hr"
            ] <> FontStyle.subHeading1 TypoGraphy
        , textView $
            [ textSize FontSize.a_14
            , text $ " · "
            , color Color.black800
            ] <> FontStyle.h1 TypoGraphy
        , textView $
            [ textSize FontSize.a_14
            , text $ state.data.baseDistance <> " km"
            , color Color.black800
            , accessibility ENABLE
            , accessibilityHint $ " and " <> state.data.baseDistance <> " hr"
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
        , onClick push $ const $ AddFirstStop
        , visibility $ boolToVisibility $ state.data.fareProductType == FPT.RENTAL
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
            , accessibilityHint $ getRideDetailsAccessibilty state
            ] <> FontStyle.body3 TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView
                ( [ textSize FontSize.a_14
                  , text $ EHC.convertUTCtoISC state.data.startTime "ddd" <> ", " <> EHC.convertUTCtoISC state.data.startTime "D" <> " " <> EHC.convertUTCtoISC state.data.startTime "MMM"
                  , color Color.black700
                  ] <> FontStyle.body1 TypoGraphy
                )
            , textView $
                [ text $ " · "
                , color Color.black800
                ] <> FontStyle.body1 TypoGraphy
            , textView $
                [ textSize FontSize.a_14
                , text $ " " <> EHC.convertUTCtoISC state.data.startTime "hh" <> ":" <> EHC.convertUTCtoISC state.data.startTime "mm" <> " " <> EHC.convertUTCtoISC state.data.startTime "a"
                , color Color.black700
                ] <> FontStyle.body1 TypoGraphy
            ]
        ]
    , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ "€" <> state.data.finalPrice
        , weight 0.1
        , gravity RIGHT
        , textSize FontSize.a_18
        ] <> FontStyle.h2 TypoGraphy

    ]
    where
    getRideDetailsAccessibilty state = getEN RIDE_STARTS_ON <> EHC.convertUTCtoISC state.data.startTime "ddd" <> ", " <> EHC.convertUTCtoISC state.data.startTime "D" <> " " <> EHC.convertUTCtoISC state.data.startTime "MMM" <> " " <> EHC.convertUTCtoISC state.data.startTime "hh" <> ":" <> EHC.convertUTCtoISC state.data.startTime "mm" <> " " <> EHC.convertUTCtoISC state.data.startTime "a" <> "and total Fare is : " <>  "€" <> state.data.finalPrice

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
        , width MATCH_PARENT
        , singleLine false
        , text $ getVarString DRIVER_WILL_BE_ASSIGNED_MINUTES_BEFORE_STARTING_THE_RIDE (singleton state.props.driverAllocationTime)  --  getString DRIVER_WILL_BE_ASSIGNED <> " " <> state.driverAllocationTime <> " " <> getString MINUTES_BEFORE_STARTING_THE_RIDE
        , accessibility ENABLE
        , accessibilityHint $ getVarString DRIVER_WILL_BE_ASSIGNED_MINUTES_BEFORE_STARTING_THE_RIDE (singleton state.props.driverAllocationTime)
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
  let cancelBookingText = case state.data.fareProductType of
        FPT.RENTAL -> getString CANCEL_RENTAL_BOOKING
        FPT.INTER_CITY -> getString CANCEL_INTERCITY_BOOKING
        _ -> getString CANCEL_BOOKING
  in
  textView $
  [ width MATCH_PARENT
  , textFromHtml $ "<u>" <> cancelBookingText <> "</u>"
  , color Color.black700
  , onClick push $ const CancelRide
  , gravity CENTER_HORIZONTAL
  , accessibility ENABLE
  , accessibilityHint cancelBookingText
  ] <> FontStyle.body3 TypoGraphy

cancelRidePopUpView :: forall w. (Action -> Effect Unit) -> RideScheduledScreenState -> PrestoDOM (Effect Unit) w
cancelRidePopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    -- , accessibility DISABLE
    ][ PopUpModal.view (push <<< CancelRideActionController) (cancelScheduledRideConfig state) ]
