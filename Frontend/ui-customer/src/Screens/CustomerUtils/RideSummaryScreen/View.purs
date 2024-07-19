module Screens.RideSummaryScreen.View where
import Data.Maybe
import Debug
import Prelude
import Screens.RideSummaryScreen.ScreenData
import Screens.RideSummaryScreen.ComponentConfig
import Components.SourceToDestination as SourceToDestination
import Components.RideSummaryCard as RideSummaryCard
import Animation as Anim
import Common.Types.App (LazyCheck(..), CategoryListType)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString,getVarString)
import PrestoDOM
import PrestoDOM.Animation as PrestoAnim
import Screens.RideSummaryScreen.Controller (Action(..), ScreenOutput, eval)
import Styles.Colors as Color
import Services.API
import Common.Types.App as CTA
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag,safeMarginTop,getCurrentUTC)
import Language.Types as LType
import Language.Strings (getString)
import JBridge as JB
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.DropDownCard as DropDownCard
import Data.Array
import Engineering.Helpers.Commons(os)
import Helpers.CommonView (emptyTextView)
import Screens.Types
import Helpers.Utils (parseFloat)
import Data.Int (toNumber)


screen :: RideSummaryScreenState -> Screen Action RideSummaryScreenState ScreenOutput
screen initialState =  
  { initialState
  , view : view
  , name: "RideSummaryScreen"
  , globalEvents : [] 
  , eval : (\action state -> do
    let _ = spy "RideRequestScreen state -----" state
    let _ = spy "RideRequestScreen action --------" action
    eval action state)
  }

view :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w 
view push state  =
  let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
  in
    Anim.screenAnimation $
    linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      ]
      ([headerLayout state push
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
          [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ if state.props.isBookingAccepted then rideScheduledView state push else emptyTextView
            , RideSummaryCard.view (rideSummaryCardConfig state.data.rideDetails) (push <<< RideSummaryCardActionController)
            , DropDownCard.view (push <<< PickUp) (dropDownCard state.props.pickUpOpen (getString LType.PICKUP_DROP) (pickUpCard state push))
            , DropDownCard.view (push <<< IncludedCharges) (dropDownCard state.props.includedChargesOpen (getString LType.INCLUDED_CHARGES) (includedChargesCard state push))
            , if tripCategory.tag /= CTA.OneWay then DropDownCard.view (push <<< ExcludedCharges) (dropDownCard state.props.excludedChargesOpen (getString LType.EXCLUDED_CHARGES) (excludedChargesCard state push)) else emptyLayout
            , DropDownCard.view (push <<< Terms) (dropDownCard state.props.termsAndConditionOpen ("Terms And Conditions") (termsAndConditionCard state push))
            , if state.props.isBookingAccepted then cancelBookingView push state else emptyTextView
            ]
          ]
      , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.grey900
          ][]
      ,  buttonLayout state push
      ])


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
      ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Ride Summary"
        -- , textSize FontSize.a_18
        , margin $ Margin 5 0 0 3
        , weight 1.0
        , color Color.black900
        ] <> FontStyle.body10 TypoGraphy
    ]


pickUpCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
pickUpCard state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
      linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          ][
          linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              ][SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state.data.rideDetails)]
          ]

googleMap :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
googleMap state push  =
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
      (LocationInformation fromLocation) = entity.fromLocation
      distanceToPickup = show (fromMaybe 0 entity.distanceToPickup)
      (LocationInformation toLocation) = fromMaybe dummyLocation entity.toLocation
    in
      frameLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          , cornerRadius 8.0
          ]
          [linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , cornerRadius 8.0
            , id (getNewIDWithTag "rideSummaryMap")
            , afterRender
                ( \action -> do
                    _ <- (JB.showMap (getNewIDWithTag "rideSummaryMap") false "satellite" (25.0) fromLocation.lat fromLocation.lon push (case tripCategory.tag of 
                                                                                                                                              CTA.InterCity -> ShowMapInterCity fromLocation.lat fromLocation.lon toLocation.lat toLocation.lon
                                                                                                                                              CTA.Rental -> ShowMapRental fromLocation.lat fromLocation.lon
                                                                                                                                              _ -> ShowMapRegular fromLocation.lat fromLocation.lon toLocation.lat toLocation.lon
                                                                                                                                          ))
                    pure unit
                )
                (const NoAction)
            ][]
          ]





includedChargesCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesCard state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
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
      plannedKMFare = (head $ filter  (\item -> (item.key == "Distance Fare ")) extraFares)
      plannedKMFareVal = case plannedKMFare of 
                                (Just plannedKMFare') -> plannedKMFare'.val
                                _ -> ""
      driverAllowance = (head $ filter  (\item -> (item.key == "Driver Allowance*")) extraFares)
      driverAllowanceVal = case driverAllowance of 
                                (Just driverAllowance') -> driverAllowance'.val
                                _ -> ""
      extraDistanceFare = (head $ filter  (\item -> (item.key == "Extra Distance Fare ")) extraFares)
      extraDistanceFareVal = case extraDistanceFare of 
                                (Just extraDistanceFare') -> extraDistanceFare'.val
                                _ -> ""
      extraTimeFare = (head $ filter  (\item -> (item.key == "Extra Time Fare ")) extraFares)
      extraTimeFareVal = case extraTimeFare of 
                                (Just extraTimeFare') -> extraTimeFare'.val
                                _ -> ""                        

      (BookingAPIEntity entity) = state.data.rideDetails
      dummyFare = RateCardItem{ price : 0, priceWithCurrency : {amount:0.0, currency:"INR"}, title : UNPLANNED_PER_KM_CHARGE }
      (RateCardItem fare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == SERVICE_CHARGE) state.data.fareDetails) !! 0) 
      (RateCardItem governmentFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == GOVERNMENT_CHARGE) state.data.fareDetails) !! 0) 
      (RateCardItem minFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == MIN_FARE) state.data.fareDetails) !! 0) 
      (RateCardItem perHourFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == PER_HOUR_CHARGE) state.data.fareDetails) !! 0) 
      (RateCardItem perMinFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == PER_MINUTE_CHARGE) state.data.fareDetails) !! 0) 
      (RateCardItem unplannedKMFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == UNPLANNED_PER_KM_CHARGE) state.data.fareDetails) !! 0) 
      (RateCardItem perHourKmFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == PER_HOUR_DISTANCE_KM) state.data.fareDetails) !! 0) 
      (RateCardItem plannedKMFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == PLANNED_PER_KM_CHARGE) state.data.fareDetails) !! 0) 
      (RateCardItem deadKMFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == DEAD_KILOMETER_FARE) state.data.fareDetails) !! 0) 
      (RateCardItem waitPerMinFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == WAITING_CHARGE_PER_MIN) state.data.fareDetails) !! 0) 
      (RateCardItem nightShiftFare) = fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == NIGHT_SHIFT_CHARGE) state.data.fareDetails) !! 0) 
      (CTA.TripCategory tripCategory) = entity.tripCategory
      includedDistance = fromMaybe 0 entity.estimatedDistance
      includedDuration = fromMaybe 0 entity.estimatedDuration
      roundTrip = fromMaybe false entity.roundTrip
    in
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , weight 1.0
        ]
        [ linearLayout
            [ width $ V $ (screenWidth unit) / 2
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
          [ if tripCategory.tag==CTA.InterCity then chargesTile "Included Distance" (formatDistance roundTrip includedDistance) true state push else chargesTile "Base Charge (10km, 1hr)" ("₹"<>(show perHourFare.price)) true state push
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                ][]
            , (case tripCategory.tag of 
                CTA.InterCity -> chargesTile "Per km charge" (plannedKMFareVal) true state push
                CTA.Rental -> chargesTile "Toll Charges" "Added at trip end" true state push
                _ -> chargesTile "Fare for 4km-10km" ("₹"<>(show fare.price)<>"/km") true state push)
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                , visibility VISIBLE
                ][]
            , chargesTile "Extra Time Charges*" (extraTimeFareVal) true state push 
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
            [ (case tripCategory.tag of 
                CTA.InterCity -> chargesTile "Included Time " (formatDuration includedDuration) true state push
                CTA.Rental -> chargesTile "Addon km charge" ("₹"<>(show plannedKMFare.price)<>"/km") true state push
                _ -> chargesTile "Pickup Charges" ("₹"<>(show deadKMFare.price)) true state push)
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                ][]
            , if tripCategory.tag==CTA.InterCity then chargesTile "Driver Allowance " (driverAllowanceVal) true state push else chargesTile "Waiting Charges" ("₹"<>(show waitPerMinFare.price)<>"/min") true state push 
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                , visibility VISIBLE
                ][]
              ,  chargesTile "Extra distance charge*" (extraDistanceFareVal) true state push
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
        formatDistance :: Boolean -> Int -> String 
        formatDistance roundTrip duration =
          let 
            mul = if roundTrip then 2.0 else 1.0
            durationDecimals = toNumber duration
            duration' = if mul*durationDecimals < 1000.0 then (parseFloat (mul*durationDecimals) 1) <> " m" else (parseFloat ((mul*durationDecimals)/1000.0) 1) <> " km" 
          in  
            duration'
          



includedChargesFooter :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesFooter state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
      nightTimeStart = entity.nightTimeStart
      nightTimeEnd = entity.nightTimeEnd
    in
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text $ getString LType.INC_1
            , color Color.black600
            , margin $ MarginBottom 12
            , visibility if tripCategory.tag == CTA.OneWay then GONE else VISIBLE
            ] <> FontStyle.paragraphText TypoGraphy
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text if tripCategory.tag == CTA.OneWay then (getString LType.INC_2A) else (getVarString LType.INC_2B [nightTimeStart,nightTimeEnd])
            , color Color.black600

            ] <> FontStyle.paragraphText TypoGraphy
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
          -- , textSize FontSize.a_12
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
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ width WRAP_CONTENT
                , height $ V 20
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
                    , text $ getString LType.TOLLS
                    , color Color.black900
                    ] <> FontStyle.subHeading2 TypoGraphy
                ]
            , linearLayout
                [ width WRAP_CONTENT
                , height $ V 20
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
                    , text $ getString LType.STATE_PERMIT
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
                    , text $ getString LType.PARKING_CHARGES
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
        , onClick push $ const $ if not state.props.isBookingAccepted then AcceptClick else  RideConfirmed
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ if not state.props.isBookingAccepted then "Confirm Booking" else "Done"
            , color Color.yellow900
            ] <> FontStyle.subHeading1 TypoGraphy
        ]



cancelBookingView :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w
cancelBookingView push state =
  let 
    bookingId = fromMaybe "" state.data.bookingId
  in
  textView $
  [ width MATCH_PARENT
  , textFromHtml $ "<u>" <> "Cancel Intercity Booking" <> "</u>"
  , color Color.black700
  , gravity CENTER_HORIZONTAL
  , margin $ Margin 16 16 16 16
  , onClick push $ const $ if bookingId /= "" then  (CancelIntercityRide bookingId) else NoAction
  -- , onClick $ push $ const $ if bookingId /= "" then (CancelIntercityRide bookingId) else NoAction
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
            text "Ride Scheduled"
          ] <> FontStyle.subHeading1 TypoGraphy,
          textView $ [  
            height WRAP_CONTENT,
            width MATCH_PARENT,
            text "Your Driver will be assigned 15 mins before the ride Starts."
          ] <> FontStyle.body3 TypoGraphy
        ]
      ]
