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
import Language.Strings (getString)
import PrestoDOM
import PrestoDOM.Animation as PrestoAnim
import Screens.RideSummaryScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Services.API
import Common.Types.App as CTA
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag , getCurrentUTC)
import Language.Types
import Language.Strings (getString)
import JBridge as JB
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.DropDownCard as DropDownCard
import Data.Array
import Data.Int as INT
import Components.PopUpModal as PopUpModal
import Mobility.Prelude (boolToVisibility)
import JBridge as JB
import Data.Function.Uncurried (runFn2)
import Effect.Uncurried (runEffectFn1)
import Timers (clearTimerWithId , startTimer,clearTimerWithIdEffect)
import PrestoDOM.Elements.Keyed as Keyed 
import Data.Tuple (Tuple(..))
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Types.App (defaultGlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import PrestoDOM.Core (getPushFn)
import Services.Backend as Remote
import Data.Array (union, (!!), filter, length, (:), foldl, drop,  replicate, updateAt, elemIndex, (..), last, find, catMaybes, sortBy, reverse)
import JBridge as JB
import Services.Backend (driverRegistrationStatusBT, dummyVehicleObject, makeDriverDLReq, makeDriverRCReq, makeGetRouteReq, makeLinkReferralCodeReq, makeOfferRideReq, makeResendAlternateNumberOtpRequest, makeTriggerOTPReq, makeValidateAlternateNumberRequest, makeValidateImageReq, makeVerifyAlternateNumberOtpRequest, makeVerifyOTPReq, mkUpdateDriverInfoReq, walkCoordinate, walkCoordinates)
import Screens.HomeScreen.ComponentConfig (mapRouteConfig)
import Constants.Configs (getPolylineAnimationConfig)
import Helpers.Utils(getCurrentLocation, LatLon(..))
import Data.Number (fromString) as Number
import Helpers.Utils as HU
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Common.Animation.Config (estimateExpandingAnimationConfig)
import Engineering.Helpers.Commons(os)
import Data.Either (Either(..), either, isRight)
import Control.Monad.Except.Trans (lift)
import Presto.Core.Flow (doAff)
import Effect.Class (liftEffect)

screen :: RideSummaryScreenState -> Screen Action RideSummaryScreenState ScreenOutput
screen initialState =  
  { initialState
  , view : view
  , name: "RideSummaryScreen"
  , globalEvents : [
     (\push -> do
        let currentTime = getCurrentUTC "" 
            tripStartTime = fromMaybe "" initialState.data.activeRideData.tripScheduledAt 
            diff = runFn2 JB.differenceBetweenTwoUTC tripStartTime currentTime  
            
        if(diff <=7800) then void $ startTimer initialState.props.timer "scheduledTimer" "60" push ScheduleTimer
        else pure unit
        push $ NoAction       
        _ <- HU.storeCallBackForNotification push Notification
        pure $ runEffectFn1 clearTimerWithIdEffect "scheduledTimer"
      )
  ] <> if (isNothing initialState.data.route) then [(\push -> do
        void $ 
          launchAff $
            EHC.flowRunner defaultGlobalState $ 
                runExceptT $ runBackT $
                  do
                    let (BookingAPIEntity entity) = initialState.data.rideDetails
                        (CTA.TripCategory tripCategory) = entity.tripCategory
                        (Location fromLocation) = entity.fromLocation
                        distanceToPickup = show (fromMaybe 0 entity.distanceToPickup)
                        (Location toLocation) = fromMaybe dummyLocation entity.toLocation
                        tag = tripCategory.tag
                    routeApiResponse <- case tag of 
                      CTA.InterCity -> do 
                             lift $ lift $ Remote.getRoute (Remote.makeGetRouteReq fromLocation.lat fromLocation.lon toLocation.lat toLocation.lon) "pickup"
                      CTA.Rental -> do 
                        (LatLon lat lon _)  <- getCurrentLocation 0.0 0.0 0.0 0.0 400 false true
                        let currentDriverLat = fromMaybe 0.0 $ Number.fromString $ lat
                            currentDriverLon = fromMaybe 0.0 $ Number.fromString $ lon
                        lift $ lift $ Remote.getRoute  (Remote.makeGetRouteReq currentDriverLat currentDriverLon fromLocation.lat fromLocation.lon  ) "pickup"
                      _           -> do
                        (LatLon lat lon _) <- getCurrentLocation 0.0 0.0 0.0 0.0 400 false true
                        let currentDriverLat = fromMaybe 0.0 $ Number.fromString $ lat
                        let currentDriverLon = fromMaybe 0.0 $ Number.fromString $ lon
                        lift $ lift $ Remote.getRoute  (Remote.makeGetRouteReqArray [LatLong{lat :currentDriverLat, lon: currentDriverLon}, LatLong{lat: fromLocation.lat ,lon: fromLocation.lon} ,LatLong{lat :toLocation.lat , lon:toLocation.lon }]) "pickup"   
                    case routeApiResponse of
                        Right (GetRouteResp val)  -> do 
                            let shortRoute = (val !! 0)
                            case shortRoute of
                              Just route -> void $ liftFlowBT $ push $ UpdateRoute route 
                              Nothing -> pure unit
                            pure unit
                              
                        Left (err) -> lift $ lift $ doAff do liftEffect $ push $ ApiError                              
        pure $ pure unit
      )] else []
  , eval : (\action state -> do
    let _ = spy "RideSummaryScreen state -----" state
    let _ = spy "RideSummaryScreen action --------" action
    eval action state)
  }

view :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w 
view push state  =
  let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
  in
    Anim.screenAnimation $
    Keyed.relativeLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ if state.props.throughBanner then  const $ Back else  const $ BackPressed
    ]
   ([ if not state.props.shimmerVisibility then
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
          [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [  bannerView state push 
            , RideSummaryCard.view (rideSummaryCardConfig state.data.rideDetails) (push <<< RideSummaryCardActionController)
            , DropDownCard.view (push <<< PickUp) (dropDownCard state.props.pickUpOpen (getString PICKUP_DROP) (pickUpCard state push))
            -- , DropDownCard.view (push <<< IncludedCharges) (dropDownCard state.props.includedChargesOpen (getString INCLUDED_CHARGES) (includedChargesCard state push)) --might use later 
            , if tripCategory.tag /= CTA.OneWay then DropDownCard.view (push <<< ExcludedCharges) (dropDownCard state.props.excludedChargesOpen (getString EXCLUDED_CHARGES) (excludedChargesCard state push)) else emptyLayout
            , DropDownCard.view (push <<< Terms) (dropDownCard state.props.termsAndConditionOpen (getString T_C) (termsAndConditionCard state push))
            , cancelButtonView state push 
            ]
          ]
      , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.grey900
          ][]
        ,  buttonLayout state push
        , goToPickUp state push
      ] 
      else Tuple "shimmerView" $ shimmerView state push 
    ]<>if state.props.showPopUp then [Tuple "endRidePopView" $ endRidePopView state push]  else []
     <> if state.props.errorPopUp then [Tuple "apiError" $ apiErrorPopUp state push ] else [])


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
    , gravity CENTER_HORIZONTAL
    ][headerLeft state push
    -- , headerRight state push   -- Might use later 
    ]

headerLeft :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLeft state push =
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
      , onClick push $ if state.props.throughBanner then  const $ Back else  const $ BackPressed
      , margin $ Margin 5 0 0 0
      , rippleColor Color.rippleShade
      ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString RIDE_SUMMARY
        , margin $ Margin 5 0 0 3
        , weight 1.0
        , color Color.black900
        ] <> FontStyle.body10 TypoGraphy
    ]

headerRight :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerRight state push =
  linearLayout
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ Padding 5 20 0 16
    , margin $ MarginHorizontal 45 0
    , weight 1.0
    , visibility $ boolToVisibility state.props.throughBanner
    , onClick push $ const Call
    , rippleColor Color.rippleShade
    ]
    [ imageView
      [ width $ V 20
      , height $ V 20
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_phone_filled_blue"
      , onClick push $ const $ BackPressed
      , margin $ Margin 5 0 0 0
      , rippleColor Color.rippleShade
      ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString CALL_CUSTOMER_TITLE
        , margin $ Margin 5 0 0 3
        , weight 1.0
        , color Color.blue900
        ] <> FontStyle.body1 TypoGraphy
    ]

pickUpCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
pickUpCard state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
    PrestoAnim.animationSet
    ([ Anim.fadeInWithDuration 400 true ] <>
    (if os == "IOS" then []
    else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) true ]))
     $ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          ]
          [ linearLayout
              [ width MATCH_PARENT
              , height $ if tripCategory.tag == CTA.InterCity then (V 160) else (V 246) 
              , margin $ MarginBottom 16
              , cornerRadius 8.0 
              , stroke ("1," <> Color.grey900)
              ][googleMap state push]
          , linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT

              ][SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state.data.rideDetails)]
          ]

googleMap :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
googleMap state push  =
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
      (Location fromLocation) = entity.fromLocation
      distanceToPickup = show (fromMaybe 0 entity.distanceToPickup)
      (Location toLocation) = fromMaybe dummyLocation entity.toLocation
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
            , id (getNewIDWithTag "DriverSavedLoc1")
            , afterRender
                ( \action -> do
                    _ <- (JB.showMap (getNewIDWithTag "DriverSavedLoc1") false "satellite" (25.0) fromLocation.lat fromLocation.lon push (case tripCategory.tag of 
                                                                                                                                              CTA.InterCity -> ShowMapInterCity fromLocation.lat fromLocation.lon toLocation.lat toLocation.lon
                                                                                                                                              CTA.Rental -> ShowMapRental fromLocation.lat fromLocation.lon
                                                                                                                                              _ -> ShowMapRegular fromLocation.lat fromLocation.lon toLocation.lat toLocation.lon
                                                                                                                                             ))
                    pure unit
                )
                ( const NoAction )
            ][]
          , mapPillView state push
          ]


mapPillView :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
mapPillView  state push =
 let 
      (BookingAPIEntity entity) = state.data.rideDetails
      distanceToPickup = fromMaybe 0 entity.distanceToPickup
  in
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , background Color.green900
  , gravity CENTER
  , padding $ Padding 12 8 12 8 
  , cornerRadii  (Corners 8.0 true false true false)
  , visibility $ boolToVisibility $ not state.props.throughBanner
  ]
  [ textView $ 
    [ text ( getString PICK_UP <>" : " <>  (JB.fromMetersToKm (distanceToPickup)) <> getString AWAY)
    , color $ Color.white900
    ] <> FontStyle.subHeading2 LanguageStyle
  ]



includedChargesCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesCard state push = 
    PrestoAnim.animationSet
    ([ Anim.fadeInWithDuration 200 true ] <>
    (if os == "IOS" then []
    else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) true ]))
  $ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , pivotY 0.0
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
      (BookingAPIEntity entity) = state.data.rideDetails
      
      
      (RateCardItem fare) = getFare SERVICE_CHARGE
      (RateCardItem governmentFare) = getFare GOVERNMENT_CHARGE
      (RateCardItem minFare) = getFare MIN_FARE
      (RateCardItem perHourFare) = getFare PER_HOUR_CHARGE
      (RateCardItem perMinFare) = getFare PER_MINUTE_CHARGE
      (RateCardItem unplannedKMFare) = getFare UNPLANNED_PER_KM_CHARGE
      (RateCardItem perHourKmFare) = getFare PER_HOUR_DISTANCE_KM
      (RateCardItem plannedKMFare) = getFare PLANNED_PER_KM_CHARGE
      (RateCardItem deadKMFare) = getFare DEAD_KILOMETER_FARE
      (RateCardItem waitPerMinFare) = getFare WAITING_CHARGE_PER_MIN
      (RateCardItem waitPerMinChargeFare) = getFare WAITING_CHARGE_RATE_PER_MIN
      (RateCardItem nightShiftFare) = getFare NIGHT_SHIFT_CHARGE
      (CTA.TripCategory tripCategory) = entity.tripCategory
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
            [ if tripCategory.tag==CTA.InterCity then chargesTile (getString  PER_KM_CHARGE) ("₹"<>(show plannedKMFare.price)<>"/km") true state push else chargesTile (getString $ BASE_CHARGE "(10km, 1hr)") ("₹"<>(show perHourFare.price)) true state push
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                ][]
            , (case tripCategory.tag of 
                CTA.InterCity -> chargesTile (getString EXTRA_TIME_CHARGE) ("₹"<>(show perMinFare.price)<>"/hr") true state push
                CTA.Rental -> chargesTile (getString TOLL_CHARGES) (getString ADDED_AT_END_OF_TRIP) true state push
                _ -> chargesTile (getString $ FARE_FOR "4km -10km") ("₹"<>(show fare.price)<>"/km") true state push)
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                , visibility $ boolToVisibility $  tripCategory.tag /= CTA.InterCity 
                ][]
            , if tripCategory.tag==CTA.Rental then chargesTile (getString EXTRA_TIME_CHARGE) ("₹"<>(show perMinFare.price)<>"/min") (tripCategory.tag /= CTA.InterCity) state push else chargesTile (getString $ FARE_FOR "10+km") ("₹"<>(show fare.price)<>"/km") (tripCategory.tag /= CTA.InterCity) state push
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
                CTA.Rental -> chargesTile (getString ADD_ON_KM_CHARGE) ("₹"<>(show plannedKMFare.price)<>"/km") true state push
                _ -> chargesTile (getString PICKUP_CHARGE) ("₹"<>(show deadKMFare.price)) true state push)
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                ][]
            , if tripCategory.tag==CTA.InterCity then chargesTile (getString EXTRA_DISTANCE_CHARGES) ("₹"<>(show unplannedKMFare.price)<>"/km") true state push else chargesTile (getString WAITING_CHARGES) ("₹"<>(show waitPerMinFare.price)<>"/min") true state push 
            , linearLayout 
                [ width WRAP_CONTENT
                , height $ V 24
                , weight 1.0
                , visibility if tripCategory.tag == CTA.InterCity then GONE else VISIBLE
                ][]
            , if tripCategory.tag==CTA.Rental then chargesTile (getString EXTRA_DISTANCE_CHARGES) ("₹"<>(show unplannedKMFare.price)<>"/km") (tripCategory.tag /= CTA.InterCity) state push else chargesTile (getString TOLL_CHARGES) ("₹"<>(show fare.price)) (tripCategory.tag /= CTA.InterCity) state push
            ]
        ]
      
    where 
      

      getFare :: TitleTag -> RateCardItem
      getFare title = 
        let dummyFare = RateCardItem { price: 0, priceWithCurrency: { amount: 0.0, currency: "INR" }, title: UNPLANNED_PER_KM_CHARGE }
        in fromMaybe dummyFare ((filter (\(RateCardItem x) -> x.title == title) state.data.fareDetails) !! 0)



includedChargesFooter :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesFooter state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
    PrestoAnim.animationSet
    ([ Anim.fadeInWithDuration 200 true ] <>
    (if os == "IOS" then []
    else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) true ]))
     $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , pivotY 0.0
        ]
        [ textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text $ getString INC_1
            , color Color.black600
            , margin $ MarginBottom 12
            , visibility if tripCategory.tag == CTA.OneWay then GONE else VISIBLE
            ] <> FontStyle.paragraphText TypoGraphy
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text if tripCategory.tag == CTA.OneWay then (getString $ INC_2A  "10am" "5pm" "1.1x") else (getString $ INC_2B "10am" "5pm" )
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
    PrestoAnim.animationSet
    ([ Anim.fadeInWithDuration 200 true ] <>
    (if os == "IOS" then []
    else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) true ]))
     $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , pivotY 0.0
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
                , visibility $ boolToVisibility $ tripCategory.tag == CTA.InterCity 
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
                    , text $ getString TOLLS
                    , color Color.black900
                    ] <> FontStyle.subHeading2 TypoGraphy
                ]
            , linearLayout
                [ width WRAP_CONTENT
                , height $ V 20
                , orientation HORIZONTAL
                , margin $ MarginBottom 24
                , visibility $ boolToVisibility $ tripCategory.tag == CTA.InterCity 
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
                    , text $ getString STATE_PERMIT
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
                    , text $ getString PARKING_CHARGE
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
            , text $ getString EXCLUDED_FOOTER
            , color Color.black600
            ] <> FontStyle.paragraphText TypoGraphy 
        ]




termsAndConditionCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
termsAndConditionCard state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
    PrestoAnim.animationSet
    ([ Anim.fadeInWithDuration 200 true ] <>
    (if os == "IOS" then []
    else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) true ]))
      $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , pivotY 0.0
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
                , text $ if tripCategory.tag == CTA.OneWay then (getString TERM_1A) else (getString $ TERM_1B "1")
                , color Color.black600
                , margin $ MarginBottom 12
                ] <> FontStyle.paragraphText TypoGraphy
            , textView $ 
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ if tripCategory.tag == CTA.OneWay then (getString TERM_2A) else (getString $ TERM_2B "40")
                , color Color.black600
                , margin $ MarginBottom 12
                ] <> FontStyle.paragraphText TypoGraphy
            , textView $ 
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ if tripCategory.tag == CTA.OneWay then (getString TERM_3A) else (getString $ TERM_3B "10km" "30")
                , color Color.black600

                ] <> FontStyle.paragraphText TypoGraphy

            ]
        ]



buttonLayout :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
buttonLayout state push = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 16 16 16 16
    , gravity CENTER
    , visibility $ boolToVisibility $ not state.props.throughBanner
    ]
    [ passButton state push 
    , linearLayout
        [ width $ V 20
        , height WRAP_CONTENT
        ][]
    , acceptButton state push 
    ]


acceptButton :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
acceptButton state push = 
    let 
      (BookingAPIEntity entity) = state.data.rideDetails
      (CTA.TripCategory tripCategory) = entity.tripCategory
    in
      linearLayout 
        [ width $ V 232
        , height WRAP_CONTENT
        , padding $ Padding 16 18 16 18
        , cornerRadius 8.0
        , gravity CENTER
        , background case tripCategory.tag of
                        CTA.InterCity -> Color.blue800
                        CTA.Rental -> Color.blueGreen
                        CTA.OneWay -> Color.green900
                        _ -> Color.blue600
        , onClick push $ const AcceptClick
        , rippleColor Color.rippleShade
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getString ACCEPT
            , color Color.white900
            ] <> FontStyle.body8 TypoGraphy
        ]





passButton :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
passButton state push = 
  linearLayout
    [ width   WRAP_CONTENT
    , height MATCH_PARENT
    , stroke ("1," <> Color.black500)
    , padding $ Padding 24 18 24 18
    , cornerRadius 8.0
    , onClick push $ const $ BackPressed
    , gravity CENTER
    , rippleColor Color.rippleShade
    ]
    [ textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString BACK
        ] <> FontStyle.subHeading1 TypoGraphy
    ]



cancelButtonView :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cancelButtonView  state push = 
  linearLayout [
    height WRAP_CONTENT
    ,width MATCH_PARENT
    , gravity CENTER_HORIZONTAL
    , padding $ PaddingVertical 10 20 
    , onClick push $ const CancelClicked
    , visibility $ boolToVisibility $ state.props.throughBanner 
  ] [
    textView $ [
      height WRAP_CONTENT
      ,width WRAP_CONTENT
      , gravity CENTER_HORIZONTAL
      ,textFromHtml $ "<u>" <> getString CANCEL_BOOKING  <> "</u>"
    ]
  ]

endRidePopView :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
endRidePopView state push =
  linearLayout
    [ width MATCH_PARENT,
      height MATCH_PARENT,
      visibility $ boolToVisibility state.props.showPopUp
    ][ PopUpModal.view (push <<< PopUpModalCancelConfirmationAction) (cancelConfirmationConfig state )]



goToPickUp :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
goToPickUp state push = 
   linearLayout [
        height $  WRAP_CONTENT 
        , width MATCH_PARENT
        , background Color.black900
        , margin $ Margin 10 10 10 10 
        , padding $ PaddingVertical 4 4 
        , cornerRadius  8.0
        , gravity CENTER
        ,  onClick push $ if checkRemainingSeconds state then (const $  GoToMap state.data.activeRideData.dest_lat state.data.activeRideData.dest_lon ) else (const  OnClick)
        , rippleColor Color.rippleShade
        , visibility $ boolToVisibility state.props.throughBanner
    ][
        imageView [
          height $ V 15
         ,width $ V 15
         ,margin $ Margin 5 5 8 5
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_go_to_pickup"
        , visibility $ boolToVisibility $ checkRemainingSeconds state
        ]
        ,textView $ [
            width WRAP_CONTENT
            , height $ V 40
            , text $ if checkRemainingSeconds state then getString GO_TO_PICKUP else getString DONE
            , color Color.yellow900
            , gravity CENTER
        ]<>FontStyle.h3 TypoGraphy
    ]

bannerView :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
bannerView  state push  = 
     linearLayout [
      height WRAP_CONTENT
     , width WRAP_CONTENT
     , orientation HORIZONTAL
     , margin $ MarginVertical 8 0
     , visibility $ boolToVisibility state.props.throughBanner
     , gravity CENTER_HORIZONTAL
     
     ] [linearLayout[
       height WRAP_CONTENT
     , width WRAP_CONTENT
     , margin $ MarginHorizontal 20 20 
     , background  Color.blue600 
     , padding $ Padding 8 8 10 8 
     , cornerRadius 12.0
      ]
      [
      imageView [
         height $ V 80
       , width  $ V 80
       , margin $ Margin 0 12 10 0 
       , imageWithFallback $ fetchImage FF_ASSET "ny_ic_ride_scheduled_clock"
      ]
     ,linearLayout[
          height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
      ][
       textView $ [
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , color Color.black800
      , text $ getString RIDE_SCHEDULED
      ]<>FontStyle.h3 TypoGraphy
      , textView $ [
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin $ MarginVertical 4 0 
      , text $ getString $ PLEASE_BE_ONLINE "40"
      ]<>FontStyle.body1 TypoGraphy 
      , textView [
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ getString BEFORE_THE_RIDE_STARTS
      ]
      ,textView[
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , color Color.warningRed
      , margin $ Margin 0 4 2 0 
      , text $ getString TRIP_WILL_BE_ASSIGNED_TO_ANOTHER_DRIVER
      ]

     ]
     ]
    ]


checkRemainingSeconds :: RideSummaryScreenState -> Boolean
checkRemainingSeconds state =  
   let currentTime = getCurrentUTC "" 
       tripStartTime = fromMaybe "" state.data.activeRideData.tripScheduledAt
  in runFn2 JB.differenceBetweenTwoUTC tripStartTime currentTime <= 1800
                


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


apiErrorPopUp :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
apiErrorPopUp state push =
  linearLayout
    [ width MATCH_PARENT,
      height MATCH_PARENT,
      visibility $ boolToVisibility state.props.errorPopUp
    ][ PopUpModal.view (push <<< PopUpModalErrorAction) (errorPopUpConfig state )]

















