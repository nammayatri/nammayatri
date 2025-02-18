module Screens.RideSummaryScreen.View where

import Data.Array
import Data.Maybe
import Debug
import Prelude
import PrestoDOM
import Screens.RideSummaryScreen.ComponentConfig
import Screens.RideSummaryScreen.ScreenData
import Screens.Types
import Services.API

import Accessor (_isScheduled, _status)
import Animation as Anim
import Common.Types.App (LazyCheck(..), CategoryListType)
import Common.Types.App as CTA
import Components.DropDownCard as DropDownCard
import Components.PopUpModal as PopUpModal
import Components.RideSummaryCard as RideSummaryCard
import Components.SourceToDestination as SourceToDestination
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (os)
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag, safeMarginTop, getCurrentUTC, flowRunner, screenHeight)
import Engineering.Helpers.Utils (toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.CommonView (emptyTextView)
import Helpers.Utils (fetchImage, FetchImageFrom(..), storeCallBackCustomer, makeNumber, decodeError)
import JBridge as JB
import Language.Strings (getString)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Language.Types as LType
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Prim.Ordering (LT)
import Screens (ScreenName(..), getScreen) as Screen
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.RideSummaryScreen.Controller (Action(..), ScreenOutput, eval)
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import ConfigProvider
import PrestoDOM.Elements.Keyed as Keyed 
import Data.Tuple (Tuple(..))

screen :: RideSummaryScreenState -> Screen Action RideSummaryScreenState ScreenOutput
screen initialState =  
  { initialState
  , view : view
  , name: "RideSummaryScreen"
  , globalEvents : [globalEventsFn] 
  , eval : (\action state -> do
    let _ = spy "RideRequestScreen state -----" state
    let _ = spy "RideRequestScreen action --------" action
    eval action state
    ) 
  }
  where 
      globalEventsFn push = do 
        void $ storeCallBackCustomer push NotificationListener "RideSummaryScreen" Just Nothing
        if any ( _ == initialState.data.fromScreen) [(Screen.getScreen Screen.HOME_SCREEN),(Screen.getScreen Screen.MY_RIDES_SCREEN)]  then do
            void $ launchAff $ flowRunner defaultGlobalState $ getBooking GetBooking CheckFlowStatusAction push initialState
        else pure unit            
        pure $ pure unit

      getBooking :: forall action. (RideBookingRes -> String-> action) -> action -> (action -> Effect Unit) -> RideSummaryScreenState -> Flow GlobalState Unit
      getBooking action flowStatusAction push state = do
        let 
          bookingId = fromMaybe "" state.data.bookingId
        if(bookingId /= "") then do
          rideBookingResponse <- Remote.rideBooking bookingId
          case rideBookingResponse of
            Right response -> do
              let
                 (RideBookingRes resp) = response
              doAff do liftEffect $ push $ action response "success"
            Left err -> do
              let errResp = err.response
                  codeMessage = decodeError errResp.errorMessage "errorMessage"
                  response = dummyRideBooking
              doAff do liftEffect $ push $ action response "failure"
        else do
              let 
                response = dummyRideBooking
              doAff do liftEffect $ push $ action response "failure"

view :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w 
view push state  =
  let 
      (BookingAPIEntity entity) = state.data.rideDetails
      startTime = entity.startTime
      (CTA.TripCategory tripCategory) = entity.tripCategory
      pickupCardHeader = case tripCategory.tag of 
                          CTA.Rental -> getString LType.PICKUP 
                          _ -> getString LType.PICKUP_DROP
      hasApiFailed = state.props.hasApiFailed
  in
    Anim.screenAnimation $
    Keyed.relativeLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const $ BackPressed $ startTime
    ]
   ( [ if not state.props.shimmerVisibility then
    Tuple "DefaultLayout" $ linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      ]
      [ headerLayout state push
      , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.grey900
          ][]
      , scrollView
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , weight 1.0
          ]
          [ if (not hasApiFailed) then
              ( linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , orientation VERTICAL
              , visibility $ boolToVisibility (not hasApiFailed)
              ]
                  [ if null state.data.rideList then emptyTextView else (driverDetailsView push state)
                  , if null state.data.rideList && state.props.isBookingAccepted then rideScheduledView state push else emptyTextView
                  , RideSummaryCard.view (rideSummaryCardConfig state.data.rideDetails) (push <<< RideSummaryCardActionController)
                  , DropDownCard.view (push <<< PickUp) (dropDownCard state.props.pickUpOpen (pickupCardHeader) (pickUpCard state push))
                  , DropDownCard.view (push <<< IncludedCharges) (dropDownCard state.props.includedChargesOpen  (getString LType.TRIP_FARE_INCLUDES) (includedChargesCard state push))
                  , if tripCategory.tag /= CTA.OneWay then DropDownCard.view (push <<< ExcludedCharges) (dropDownCard state.props.excludedChargesOpen (getString LType.TRIP_FARE_EXCLUDES) (excludedChargesCard state push)) else emptyLayout
                  , if state.props.isBookingAccepted then cancelBookingView push state else emptyTextView
                  ])
            else 
            ( linearLayout[
                  width MATCH_PARENT
                , height MATCH_PARENT
                , orientation VERTICAL
                , gravity CENTER_VERTICAL
                , visibility $ boolToVisibility hasApiFailed
                , padding $ PaddingTop ((screenHeight unit)/4)
              ][
                errorView state
              ]
            )
          ]
          , linearLayout
              [ width MATCH_PARENT
              , height $ V 1
              , background Color.grey900
              ][]
            ,  buttonLayout state push
            ] 
          else  Tuple "shimmerView" $ shimmerView state push 
          ]
           <> if state.props.isCancelRide then [Tuple "CancelRidePopUp" $ cancelRidePopUpView push state] else []
           <> if state.props.showCallPopUp then [Tuple "DriverCallPopUp" $ driverCallPopUp push state] else [])



emptyLayout :: forall w . PrestoDOM (Effect Unit) w
emptyLayout = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ][]

headerLayout :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , background Color.white900
    , padding if os == "IOS" then (PaddingTop safeMarginTop) else (PaddingTop 0)
    ][headerLeft state push]

headerLeft :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLeft state push =
  let 
    (BookingAPIEntity entity) = state.data.rideDetails
    startTimeUTC = entity.startTime
  in
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
      , onClick push $ const $ BackPressed startTimeUTC
      , margin $ Margin 5 0 0 0
      , rippleColor Color.rippleShade
      , accessibility ENABLE
      , accessibilityHint "Go Back "
      ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString LType.RIDE_SUMMARY
        -- , textSize FontSize.a_18
        , margin $ Margin 5 0 0 3
        , weight 1.0
        , color Color.black900
        , accessibility ENABLE
        , accessibilityHint "Ride Summary "
        ] <> FontStyle.body10 TypoGraphy
    ]


pickUpCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
pickUpCard state push = 
    linearLayout[  
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginTop 2
      ][SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state.data.rideDetails)]





includedChargesCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesCard state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 2
    ]
    [ includedChargesBox state push 
    , linearLayout
        [width MATCH_PARENT
        , height $ V 20
        ][]
    , includedChargesFooter state push 
    ]
includedChargesBox :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesBox state push =  
    let 
      extraFares = state.data.extraFare                     
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
      includedDistance = fromMaybe "" entity.estimatedDistance
      includedDuration = fromMaybe 0 entity.estimatedDuration
      roundTrip = fromMaybe false entity.roundTrip
    in
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ]
        [ linearLayout
            [ width $ V $ (screenWidth unit) / 2
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
          [chargesTile (getString LType.DISTANCE )(includedDistance) true state push 
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , weight 1.0
            ][]
        , linearLayout
            [ width  $ V $ (screenWidth unit) / 2
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ chargesTile (getString LType.TIME)(formatDuration includedDuration) true state push
            ]
        ]
        where 
        formatDuration :: Int -> String
        formatDuration duration =
          let days = duration / (60 * 60 * 24)
              remainingAfterDays = duration `mod` (60 * 60 * 24)
              hours = remainingAfterDays / (60 * 60)
              remainingAfterHours = remainingAfterDays `mod` (60 * 60)
              minutes = remainingAfterHours / 60
              daysStr = if days > 0 then show days <> (if days > 1 then " days " else " day ") else ""
              hoursStr = if hours > 0 then show hours <> " hrs " else ""
              minutesStr = if minutes > 0 then show minutes <> " mins " else ""
          in daysStr <> hoursStr <> minutesStr

includedChargesFooter :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesFooter state push = 
    let 
      extraFares = state.data.extraFare
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
      extraDistanceFare = (head $ filter  (\item -> (item.key == "EXTRA_DISTANCE_FARE")) extraFares)
      extraDistanceFareVal = case extraDistanceFare of 
                                (Just extraDistanceFare') -> extraDistanceFare'.val
                                _ -> ""
      extraTimeFare = (head $ filter  (\item -> (item.key == "EXTRA_TIME_FARE")) extraFares)
      extraTimeFareVal = case extraTimeFare of 
                                (Just extraTimeFare') -> extraTimeFare'.val
                                _ -> "" 
    in
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , background $ Color.blue600
        , cornerRadius 8.0
        , visibility $ boolToVisibility $ (not $ DS.null extraDistanceFareVal) && (not $ DS.null extraTimeFareVal)
        ]
        [ 
          imageView
            [ width $ V 20
            , height $ V 20
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info"
            , margin $ Margin 8 8 8 8
            , gravity CENTER_VERTICAL
            , color Color.black900
            ]
          , linearLayout[
            height WRAP_CONTENT,
            width MATCH_PARENT,
            orientation VERTICAL
          ][
            textView $ 
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text $ getString LType.EXTRAS_WILL_BE_CHARGED_AT
              , color Color.black700
              , margin $ Margin 2 0 2 2 
              ] <> FontStyle.body1 TypoGraphy
            , textView $ 
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text $ ((getCurrency appConfig) <>extraDistanceFareVal <> " " <>(getString LType.PER_KM)) <> (getString LType.AND) <> ((getCurrency appConfig) <>extraTimeFareVal <> " " <> (getString LType.PER_MIN)) 
              , color Color.black700
              , margin $ Margin 2 0 2 2
              ] <> FontStyle.body1 TypoGraphy
          ]
        ]


chargesTile :: String -> String -> Boolean -> RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
chargesTile text1 text2 isVisible state push = 
    linearLayout 
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , visibility if isVisible then VISIBLE else GONE
      ]
      [ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text text1
          , color Color.black600
          , textSize FontSize.a_14
          ]
      , textView $ 
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text text2
          , color Color.black900
          ] <> FontStyle.subHeading2 TypoGraphy
      ]





excludedChargesCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
excludedChargesCard state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginTop 2
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , margin $ MarginBottom 24
                , visibility if tripCategory.tag == CTA.InterCity then VISIBLE else GONE
                ]
                [ imageView
                    [ width $ V 20
                    , height $ V 20
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_car_8"
                    , margin $ MarginRight 8 
                    , color Color.black900
                    ]
                , textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString LType.TOLL_CHARGES
                    , color Color.black900
                    ] <> FontStyle.subHeading2 TypoGraphy
                ]
            , linearLayout
                [ width WRAP_CONTENT
                , height $ WRAP_CONTENT
                , orientation HORIZONTAL
                , margin $ MarginBottom 24
                , visibility if tripCategory.tag == CTA.InterCity then VISIBLE else GONE
                ]
                [ imageView
                    [ width $ V 20
                    , height $ V 20
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_certificate"
                    , margin $ MarginRight 8 
                    , color Color.black900
                    ]
                , textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString LType.STATE_PERMIT_CHARGES
                    , color Color.black900
                    ] <> FontStyle.subHeading2 TypoGraphy
                ]
            , linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                ]
                [ imageView
                    [ width $ V 20
                    , height $ V 20
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_parking"
                    , margin $ MarginRight 8 
                    , color Color.black900
                    ]
                , textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString LType.PARKING_CHARGE
                    , color Color.black900
                    ] <> FontStyle.subHeading2 TypoGraphy
                ]
            ]
        , linearLayout 
            [ width MATCH_PARENT
            , height $ V 20
            ][]
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text $ getString LType.EXCLUDED_FOOTER
            , color Color.black600
            ] <> FontStyle.paragraphText TypoGraphy 
        ]




termsAndConditionCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
termsAndConditionCard state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , visibility if state.props.termsAndConditionOpen then VISIBLE else GONE 
            ]
            [ textView $ 
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ if tripCategory.tag == CTA.OneWay then (getString LType.TERM_1A) else (getString LType.INTERCITY_TC_1)
                , color Color.black600
                , margin $ MarginBottom 12
                ] <> FontStyle.paragraphText TypoGraphy
            , textView $ 
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ if tripCategory.tag == CTA.OneWay then (getString LType.TERM_2A) else (getString LType.INTERCITY_TC_2)
                , color Color.black600
                , margin $ MarginBottom 12
                ] <> FontStyle.paragraphText TypoGraphy
            ]
        ]



buttonLayout :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
buttonLayout state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 16 16 16 16
    , gravity CENTER
    ]
    [ 
      acceptButton state push 
    ]


acceptButton :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
acceptButton state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
      linearLayout 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 16 18 16 18
        , cornerRadius 8.0
        , gravity CENTER
        , background case tripCategory.tag of
                        CTA.InterCity -> Color.black900
                        _ -> Color.black900
        , onClick push $ const $ if not state.props.isBookingAccepted && not state.props.hasApiFailed then AcceptClick 
                                else if state.props.hasApiFailed then RefreshScreen
                                else RideConfirmed
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ if not state.props.isBookingAccepted && not state.props.hasApiFailed then((getString LType.CONFIRM) <> " " <> (getString LType.INTER_CITY_)) 
                    else if state.props.hasApiFailed  then (getString LType.TRY_AGAIN)
                    else (getString LType.DONE)
            , color Color.yellow900
            ] <> FontStyle.subHeading1 TypoGraphy
        ]



cancelBookingView :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w
cancelBookingView push state =
  let 
    bookingId = fromMaybe "" state.data.bookingId
    (BookingAPIEntity entity) = state.data.rideDetails
    (CTA.TripCategory tripCategory) = entity.tripCategory
    cancelText = case tripCategory.tag of 
                    CTA.Rental ->getString LType.RENTAL_STR
                    CTA.InterCity ->getString LType.INTER_CITY_
                    CTA.OneWay -> getString LType.ONE_WAY_STR
                    _ -> getString LType.ONE_WAY_STR
  in
  textView $
  [ width MATCH_PARENT
  , textFromHtml $ "<u>" <>( getString LType.CANCEL_STR) <>" "<> cancelText <>" " <>(getString LType.BOOKING) <> "</u>"
  , color Color.black700
  , gravity CENTER_HORIZONTAL
  , margin $ Margin 16 16 16 16
  , onClick push $ const $ if bookingId /= "" then CancelRide else NoAction
  ] <> FontStyle.body2 TypoGraphy


passButton :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
passButton state push = 
  linearLayout
    [ width $ V 84
    , height MATCH_PARENT
    , stroke ("1," <> Color.black500)
    , padding $ Padding 24 18 24 18
    , cornerRadius 8.0
    , gravity CENTER
    ]
    [ textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString LType.PASS
        ] <> FontStyle.subHeading1 TypoGraphy
    ]


rideScheduledView :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w 
rideScheduledView state push = 
  let
      (BookingAPIEntity entity) = state.data.rideDetails
  in
  linearLayout
    [
      width MATCH_PARENT,
      height WRAP_CONTENT,
      padding $ Padding 16 16 16 16,
      margin $ Margin 16 16 16 0,
      cornerRadius 8.0,
      visibility (if entity.startTime > (getCurrentUTC "") then VISIBLE else GONE),
      background $ Color.blue600
    ][
        imageView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_ride_scheduled_square"
          , margin $ MarginRight 8
          , color Color.black900
          ],
        linearLayout
        [
          height WRAP_CONTENT,
          width MATCH_PARENT,
          orientation VERTICAL
        ][
          textView $ [  
            height WRAP_CONTENT,
            width MATCH_PARENT,
            text $ getString LType.RIDE_SCHEDULED
          ] <> FontStyle.subHeading1 TypoGraphy,
          textView $ [  
            height WRAP_CONTENT,
            width MATCH_PARENT,
            text $ getString LType.YOUR_DRIVER_WILL_BE
          ] <> FontStyle.body3 TypoGraphy
        ]
      ]

---------------------------------- ratingView ---------------------------------------

driverDetailsView :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w
driverDetailsView push state = 

  linearLayout[
      height WRAP_CONTENT,
      width MATCH_PARENT,
      background $ Color.blue600,
      margin $ Margin 16 16 16 0, 
      padding $ Padding 16 16 16 16,
      cornerRadius $ 24.0
  ][
    linearLayout[
      height WRAP_CONTENT,
      width MATCH_PARENT,
      orientation VERTICAL
    ][
      driverInfoView push state
    , whiteSeperator
    , carDetailsView state
    ]
  ]

carDetailsView ::  forall w. RideSummaryScreenState -> PrestoDOM (Effect Unit) w 
carDetailsView state = 
  let 
    (RideAPIEntity driverInfo) = fromMaybe dummyRideAPIEntity (head $ state.data.rideList)   
  in 
  linearLayout[
    height WRAP_CONTENT,
    width MATCH_PARENT,
    orientation HORIZONTAL
  ][ textView $ 
   [ height WRAP_CONTENT
    , weight 1.0
    , singleLine false 
    , maxLines 2
    , text $  driverInfo.vehicleColor <> " " <> driverInfo.vehicleModel
    , margin $ Margin 4 8 4 8
    , color $ Color.black700
    ] <> FontStyle.paragraphText TypoGraphy
   , vehiclePlateView state
  ]

vehiclePlateView :: forall w. RideSummaryScreenState -> PrestoDOM (Effect Unit) w
vehiclePlateView state =  
  let 
    (RideAPIEntity driverInfo) = fromMaybe dummyRideAPIEntity (head $ state.data.rideList)   
    vehicleNumber = driverInfo.vehicleNumber
  in 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , accessibility ENABLE
    , gravity RIGHT
    , accessibilityHint $ "Vehicle Number " <> (DS.replaceAll (DS.Pattern "") (DS.Replacement " ") vehicleNumber)
    , accessibility DISABLE_DESCENDANT
    , margin $ MarginTop 8
    ][linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ][ linearLayout
      [ height $ V 38
      , width MATCH_PARENT
      , background $ Color.yellow900
      , cornerRadius 4.0
      , orientation HORIZONTAL
      , margin $ MarginRight 2
      , padding $ Padding 2 2 2 2
      , alignParentBottom "true,-1"
      ][linearLayout
        [ height $ V 34
        , width MATCH_PARENT
        , stroke $ "2," <> Color.black
        , cornerRadius 4.0
        , orientation HORIZONTAL
        ][  imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_number_plate"
            , gravity LEFT
            , visibility $ VISIBLE
            , background "#1C4188"
            , height MATCH_PARENT
            , width $ V 22
            ]
            , textView $
            [  margin $ MarginHorizontal 4 4
              , singleLine true 
              , maxLines 1 
              , height MATCH_PARENT
              , text $ (makeNumber vehicleNumber)
              , color Color.black800
              , fontStyle $ FontStyle.feFont LanguageStyle
              , gravity CENTER
              , textSize FontSize.a_14
              ]
            ]
          ]
        ]
      ]
whiteSeperator :: forall w. PrestoDOM (Effect Unit) w 
whiteSeperator = 
      linearLayout[
          width MATCH_PARENT
        , height $ V 2
        , background $ Color.white900
        , margin $ MarginVertical 8 8
      ][]

driverInfoView ::(Action -> Effect Unit) -> RideSummaryScreenState ->  forall w. PrestoDOM (Effect Unit) w 
driverInfoView push state = 
  let 
    (RideAPIEntity driverInfo) = fromMaybe dummyRideAPIEntity (head $ state.data.rideList)   

  in 
  linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
  ][
    frameLayout[
      height WRAP_CONTENT,
      width WRAP_CONTENT,
      orientation VERTICAL
      ][
        imageView
      [ height $ V 50
      , width $ V 50
      , padding $ Padding 2 3 2 1
      , imageWithFallback $ fetchImage FF_ASSET (case driverInfo.vehicleVariant of
                                                       "AUTO_RICKSHAW" -> "ny_ic_driver_auto_profile"
                                                       _ -> "ny_ic_driver_cab")
      ]
      , ratingView state
      ],
    linearLayout[
      height WRAP_CONTENT,
      weight 1.0,
      orientation VERTICAL,
      margin $ MarginHorizontal 12 12
    ][
      textView $ [
        text $ driverInfo.driverName,
        color $ Color.black900,
        height WRAP_CONTENT,
        width WRAP_CONTENT,
        margin $ MarginBottom 4,
        gravity LEFT
      ]<> FontStyle.subHeading1 TypoGraphy,
      textView $ [
        text $ getString LType.IS_YOUR_DRIVER,
        color $ Color.black900,
        height WRAP_CONTENT,
        width WRAP_CONTENT,
        gravity LEFT
      ]<> FontStyle.paragraphText TypoGraphy 
    ],
    imageView[
      height $ V 50,
      width $ V 50,
      padding $ Padding 4 4 4 4,
      imageWithFallback $ fetchImage FF_ASSET "ny_ic_phone_green",
      onClick push $ const $ CallDriver
    ]
  ]

ratingView :: forall w. RideSummaryScreenState -> PrestoDOM (Effect Unit) w
ratingView state =
  let 
    (RideAPIEntity driverInfo) = fromMaybe dummyRideAPIEntity (head $ state.data.rideList)   
    rating = fromMaybe 0.0 driverInfo.driverRatings
  in 
  linearLayout
  [ orientation HORIZONTAL
  , margin $ MarginTop 40
  , height $ V 19
  , width $ V 50
  , padding $ Padding 6 3 6 3
  , background $ Color.white900
  , gravity CENTER_VERTICAL
  , cornerRadius $ 12.0
  , accessibility DISABLE
  ][textView $
    [ text $ if rating == 0.0 then (getString NEW_) else show rating
    , color $ Color.black900 
    , gravity CENTER_VERTICAL
    , margin $ MarginLeft if os == "IOS" then 0 else 3
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


cancelRidePopUpView :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w
cancelRidePopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    -- , accessibility DISABLE
    ][ PopUpModal.view (push <<< CancelRideActionController) (cancelScheduledRideConfig state) ]

driverCallPopUp :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w
driverCallPopUp push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    ]
    [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT 
      , background Color.black9000
      , accessibilityHint "Call driver popup double tap to dismiss : Button"
      , accessibility ENABLE
      , disableClickFeedback true
      , onClick push (const $ CloseShowCallDialer)
      ][
        imageView[
          height $ V 5,
          width $ V 5,
          imageWithFallback $ fetchImage FF_ASSET "ny_ic_cross_white"
        ]
      ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , cornerRadii $ Corners 24.0 true true false false
        , padding (Padding 20 32 20 25)
        , alignParentBottom "true,-1"
        , disableClickFeedback true
        ]
        [ textView
            $
              [ text (getString CALL_DRIVER_USING)
              , height WRAP_CONTENT
              , color Color.black700
              , textSize FontSize.a_18
              , margin (MarginBottom 4)
              ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            ( map
                ( \item ->
                    linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ trackingCardCallView push state item
                      , if(item.type == ANONYMOUS_CALLER) then linearLayout
                          [ height $ V 1
                          , width MATCH_PARENT
                          , background Color.grey900
                          ]
                          []
                        else linearLayout[][]
                      ]
                )
                (driverCallPopUpData state)
            )
        ]
    ]


trackingCardCallView :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> { text :: String, imageWithFallback :: String, type :: CallType, data :: String} -> PrestoDOM (Effect Unit) w
trackingCardCallView push state item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding (Padding 0 20 0 20)
    , accessibility ENABLE
    , accessibilityHint $ item.text <> " : " <> item.data
    , gravity CENTER_VERTICAL
    , onClick push (const (ShowCallDialer item.type))
    ]
    [
    imageView
        [ imageWithFallback item.imageWithFallback
        , height $ V 30
        , width $ V 30
        , margin (MarginRight 20)
        ]
    ,  linearLayout[
        height WRAP_CONTENT
      , weight 1.0
      , orientation VERTICAL]
    [
      linearLayout
      [
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , orientation HORIZONTAL
      , margin (MarginBottom 2)
      ][
        textView
        $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , textSize FontSize.a_16
          , text item.text
          , gravity CENTER_VERTICAL
          , color Color.black800
          ]
        , if(item.type == ANONYMOUS_CALLER) then labelView push state else linearLayout[][]
      ]
      , textView
        $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text item.data
          , color Color.black600
          ]
    ]
    , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
        , height $ V 30
        , width $ V 32
        , padding (Padding 3 3 3 3)
        ]
    ]

labelView :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w
labelView push state =
  linearLayout[
    height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadii $ Corners 8.0 true true true true
  , background Color.green900
  , margin (MarginHorizontal 10 10)
  , visibility $ boolToVisibility $ state.data.config.showRecommendedText
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , color Color.white900
    , gravity CENTER
    , padding (Padding 8 1 8 1)
    , textSize FontSize.a_13
    , text (getString RECOMMENDED)
    ]
  ]

driverCallPopUpData :: RideSummaryScreenState -> Array { text :: String, imageWithFallback :: String, type :: CallType, data :: String }
driverCallPopUpData state =
  [ { text: (getString ANONYMOUS_CALL)
    , imageWithFallback: fetchImage FF_ASSET "ic_anonymous_call"
    , type: ANONYMOUS_CALLER
    , data: (getString YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE)
    }
  , { text: (getString DIRECT_CALL)
    , imageWithFallback: fetchImage FF_ASSET "ic_direct_call"
    , type: DIRECT_CALLER
    , data: (getString YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER)
    }
  ]

errorView :: forall w. RideSummaryScreenState -> PrestoDOM (Effect Unit) w
errorView config =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  ][  imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_error_404"
      , height $ V 110
      , width $ V 124
      , visibility $ VISIBLE
      , margin $ (MarginBottom 31)
      ]
    , textView $
      [ text (getString LType.TRY_AGAIN)
      , color $ Color.black900
      , padding $ Padding 2 2 2 2
      , gravity CENTER
      , margin $ MarginBottom 7
      , visibility VISIBLE
      ] <> (FontStyle.body3 LanguageStyle) 
    , textView $ 
      [ text $ (getString LType.SORRY_WE_COULDNT_FIND_ANY_RIDES)
      , color $ Color.black700
      , padding  (Padding 16 0 16 16)
      , margin (Margin 33 0 33 0)
      , gravity CENTER
      , visibility VISIBLE
      ] <> (FontStyle.body6 LanguageStyle) 
  ]


shimmerView ::  RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w  
shimmerView state push   =
  shimmerFrameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , color Color.white900
    ,visibility $ boolToVisibility $ state.props.shimmerVisibility
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , color Color.white900
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height (V 350)
            , width MATCH_PARENT
            , background Color.greyDark
            , cornerRadius 12.0
            , orientation VERTICAL
            ]
            []
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , margin $ MarginTop 24
            ]
            ( map
                ( \_ ->
                    linearLayout
                      [ width MATCH_PARENT
                      , height (V 60)
                      , orientation VERTICAL
                      , margin $ MarginVertical 10 10
                      , background Color.greyDark
                      , cornerRadius 12.0
                      ]
                      []
                )
                (1 .. 4)
            )
        ]
    ]
