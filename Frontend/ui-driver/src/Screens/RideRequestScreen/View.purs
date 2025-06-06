module Screens.RideRequestScreen.View where

import Animation
import Debug
import Prelude
import PrestoDOM.List
import Screens.RideRequestScreen.Controller
import Screens.RideRequestScreen.ScreenData
import Data.Maybe
import Animation as Anim
import Common.Types.App
import Data.Array as DA
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1)
import Data.Int (ceil, fromString, round, toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth, os, safeMarginBottom, getFutureDate)
import Engineering.Helpers.Commons (screenWidth)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, lottieAnimationView, margin, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, singleLine, stroke, text, textSize, textView, onScrollStateChange, visibility, weight, width, topShift, onAnimationEnd, horizontalScrollView, scrollBarX, setEnable)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, LoggableScreen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, frameLayout, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, stroke, lineHeight, visibility, afterRender, scrollBarY, imageWithFallback, rippleColor, clickable, relativeLayout, alignParentBottom, id, onAnimationEnd, swipeRefreshLayout, onRefresh, onScroll, nestedScrollView, enableRefresh)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RideRequestScreen.Controller (Action(..), ScreenOutput)
import Screens.Types as ST
import Services.API (ScheduledBookingListResponse(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (FlowBT, GlobalState(..),defaultGlobalState)
import Helpers.Utils as HU
import Foreign.Class (encode)
import DecodeUtil (decodeForeignAny, getAnyFromWindow, parseJSON, setAnyInWindow, stringifyJSON, getFromWindowString)
import Common.Types.App as CTA
import Data.Array (mapWithIndex , (!!), length, null)
import Helpers.Utils (isYesterday, LatLon(..), decodeErrorCode, decodeErrorMessage, getCurrentLocation, getDatebyCount, getDowngradeOptions, getGenderIndex, getNegotiationUnit, getPastDays, getPastWeeks, getTime, getcurrentdate, isDateGreaterThan, onBoardingSubscriptionScreenCheck, parseFloat, secondsLeft, toStringJSON, translateString, getDistanceBwCordinates, getCityConfig, getDriverStatus, getDriverStatusFromMode, updateDriverStatus, getLatestAndroidVersion, isDateNDaysAgo, getHvErrorMsg)


screen :: RideRequestScreenState -> ListItem -> LoggableScreen Action RideRequestScreenState ScreenOutput
screen initialState listItem =

    { initialState
    , view: view listItem
    , name: "RideRequestScreen"
    , globalEvents:
        [ globalOnScroll "RideRequestScreen"
        , ( \push -> do
                void $ launchAff $ EHC.flowRunner defaultGlobalState $ getRideList push initialState
                _ <- HU.storeCallBackForNotification push Notification
                _ <- JB.getCurrentPosition push UpdateCurrentLocation
                pure $ pure unit
          )
        ]
    , eval:
        ( \action state -> do
            let
              _ = spy "RideRequestScreen state -----" state
              _ = spy "RideRequestScreen action --------" action
            eval action state
        )
  , parent : Nothing
  , logWhitelist
  }

logWhitelist :: Array String
logWhitelist = []

view :: forall w. ListItem -> (Action -> Effect Unit) -> RideRequestScreenState -> PrestoDOM (Effect Unit) w
view listItem push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , onAnimationEnd push $ const $ NoAction
        , background Color.grey700
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , gravity CENTER
              ]
              [ headerLayout state push
              , navbarlayout state push
              , dateAndDayLayout state push
              , listLayout push state listItem

              ,loadButtonView state push
              ]
          ]
        )

listLayout :: forall w. (Action -> Effect Unit) -> RideRequestScreenState -> ListItem -> PrestoDOM (Effect Unit) w
listLayout push state listItm =
  let (ScheduledBookingListResponse response) = state.data.resp

  in
  swipeRefreshLayout
    ( [ weight 1.0
      , width MATCH_PARENT
      , onRefresh push (const Refresh)
      , enableRefresh state.data.refreshLoader
      ]
      <> if state.data.scrollEnable then [setEnable $ false] else []
    )
    [ Keyed.relativeLayout
       [
         height MATCH_PARENT
        ,width MATCH_PARENT
        ,orientation VERTICAL
      ]([ Tuple "Rides"
          $list
          [  width MATCH_PARENT
          , visibility VISIBLE
          , listItem listItm
          , visibility $ boolToVisibility $ DA.null response.bookings
          , onScroll "rides" "RideRequestScreen" push (Scroll)
          , onScrollStateChange push (ScrollStateChanged)
          , listDataV2  (myRideListTransformerProp state.data.filteredArr)
          ]
        , Tuple "NoRides"
        $ linearLayout[
          height $ WRAP_CONTENT
          , width $ MATCH_PARENT
          , gravity CENTER_HORIZONTAL
          , background Color.grey700
          , orientation VERTICAL
          , visibility  $ boolToVisibility $ (DA.null state.data.filteredArr ) && (state.props.noLocationFlag == false )
        ][
          imageView [
            width $ V 140
            ,height $ V 155
          , gravity CENTER_HORIZONTAL
          ,imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_no_rides"
          ]
          ,textView $[
            width $ V 290
          , height $ V 20
          , gravity CENTER
          , text $ getString CURRENTLY_THERE_ARE_NO_RIDES_AVAILABLE
          , textSize FontSize.a_14
          , color Color.black900
          ] <> FontStyle.h3 TypoGraphy
          ,textView $[
            width $ V 290
          , height $ V 20
          , gravity CENTER
          , text $ getString  PLEASE_TRY_AGAIN
          , textSize FontSize.a_14
          , color Color.black900
          ] <> FontStyle.h3 TypoGraphy

        ]
        , Tuple "NoLocation"
        $ linearLayout[
          height $ WRAP_CONTENT
          , width $ MATCH_PARENT
          , gravity CENTER
          , background Color.grey700
          , orientation VERTICAL
          , visibility  $ boolToVisibility $ (state.props.noLocationFlag ) && (DA.null state.data.filteredArr )
        ][

          textView $[
            width $ V 290
          , height $ V 20
          , gravity CENTER
          , text $ getString WE_ARE_NOT_ABLE_TO_FETCH_YOUR_CURRENT_LOCATION
          , textSize FontSize.a_14

          ] <> FontStyle.h3 TypoGraphy
          ,textView $[
            width $ V 290
          , height $ V 20
          , gravity CENTER
          , text $ getString  PLEASE_TRY_AGAIN
          , textSize FontSize.a_14

          ] <> FontStyle.h3 TypoGraphy

        ]
        ,Tuple "LOADER"
        $ PrestoAnim.animationSet
          [ PrestoAnim.Animation
            [ PrestoAnim.duration 100
            , PrestoAnim.toAlpha $
                case state.data.shimmerLoader of
                  loader | loader `DA.elem` [ST.AnimatingIn, ST.AnimatedIn] -> 1.0
                  loader | loader `DA.elem` [ST.AnimatingOut, ST.AnimatedOut] -> 0.0
                  _ -> 0.0
            , PrestoAnim.fromAlpha $
                case state.data.shimmerLoader of
                  loader | loader `DA.elem` [ST.AnimatingIn, ST.AnimatedOut] -> 0.0
                  loader | loader `DA.elem` [ST.AnimatingOut, ST.AnimatedIn] -> 1.0
                  _ -> 0.0
            , PrestoAnim.tag "Shimmer"
            ] true
          ] $ list
            [ height MATCH_PARENT
            , scrollBarY false
            , background Color.white900
            , width MATCH_PARENT
            , onAnimationEnd push OnFadeComplete
            , listItem listItm
            , listDataV2 $ shimmerData <$> (JB.getArray 5)
            , visibility $  (case state.data.shimmerLoader of
                    ST.AnimatedOut -> GONE
                    _ -> VISIBLE)
            ]
       ])

    ]

headerLayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , background Color.white900
    ]
    [ headerLeft state push
    ]

headerLeft :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
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
        , onClick push $ const $ BackPressed
        , margin $ MarginLeft 5
        , rippleColor Color.rippleShade
        ]
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString MORE_RIDES
          , textSize FontSize.a_18
          , margin $ Margin 5 0 0 2
          , weight 1.0
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    ]

navbarlayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
navbarlayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 3 4 3 4
    , margin $ Margin 16 16 16 16
    , cornerRadius 18.0
    , background Color.white900
    ]
    ( DA.mapWithIndex
        ( \index item -> navpillView state item push index
        )
        state.data.pillViewArray
    )

navpillView :: RideRequestScreenState -> PillViewConfig -> (Action -> Effect Unit) -> Int -> forall w. PrestoDOM (Effect Unit) w
navpillView state config push idx =
  linearLayout
    [ height WRAP_CONTENT
    , gravity CENTER
    , orientation HORIZONTAL
    , padding $ Padding 5 8 5 8
    , weight 1.0
    , cornerRadius 18.0
    , onClick push $ const $ RideTypeSelected config.rideType idx
    , background if idx == state.data.activeRideIndex then config.activeColor else Color.white900
    , rippleColor Color.rippleShade
    , clickable $ idx /= state.data.activeRideIndex
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.pillViewText
          , textSize FontSize.a_13
          , color if idx == state.data.activeRideIndex then Color.white900 else Color.black900
          ]
        <> FontStyle.tags TypoGraphy
    ]

daypillView :: RideRequestScreenState -> PillViewConfig -> (Action -> Effect Unit) -> Int -> forall w. PrestoDOM (Effect Unit) w
daypillView state config push idx =
  linearLayout
    [ height WRAP_CONTENT
    , gravity CENTER
    , orientation HORIZONTAL
    , padding $ Padding 12 4 12 4
    , weight 1.0
    , cornerRadius 18.0
    , background if idx == state.data.activeDayIndex then config.activeColor else Color.white900
    , onClick push $ const $ SelectDay idx
    , rippleColor Color.rippleShade
    , clickable $ idx /= state.data.activeDayIndex
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.pillViewText
          , color if idx == state.data.activeDayIndex then Color.white900 else Color.black900
          , textSize FontSize.a_13
          ]
        <> FontStyle.tags TypoGraphy
    ]
dateAndDayLayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dateAndDayLayout state push =
  let
    today = EHC.getCurrentUTC ""
    tommorow = (EHC.convertUTCtoISC (getFutureDate today 1) "DD MMM,YYYY")
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ Margin 16 16 16 16
    , gravity CENTER
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ if state.data.activeDayIndex ==1 then  tommorow else (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "DD MMM,YYYY")
          , color $ Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    , linearLayout
        [ weight 0.5
        ]
        []
    , dayLayout state push
    ]

dayLayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dayLayout state push =
  linearLayout
    [ width $ WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding $ Padding 5 3 5 3
    , cornerRadius 18.0
    , background Color.white900
    ]
    ( DA.mapWithIndex
        ( \index item -> daypillView state item push index
        )
        [
              {
                rideType : Nothing,
                pillViewText : getString TODAY,
                isSelected : true,
                activeColor : Color.black900
                },{
                 rideType : Nothing,
                pillViewText : getString TOMORROW,
                isSelected : false,
                activeColor : Color.black900
              }
]
    )

loadButtonView :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
loadButtonView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , gravity CENTER
    , padding $ PaddingBottom 5
    , onClick push $ const Loader
    , visibility $ boolToVisibility $ state.data.loaderButtonVisibility && (not state.data.loadMoreDisabled)
    ]
    [ linearLayout
        [ background Color.grey900
        , width MATCH_PARENT
        , height $ V 1
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , padding $ PaddingVertical 5 5
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString VIEW_MORE
              , padding (Padding 10 5 10 5)
              , color Color.blue900
              ]
            <> FontStyle.body1 LanguageStyle
        ]
    ]

getRideList :: (Action -> Effect Unit) -> RideRequestScreenState -> Flow GlobalState Unit
getRideList push state = do
      when state.props.shouldCall $ do
        let rideType  =   getRideType state
            tripCategory =  if  rideType == "InterCity" then  rideType<>"_OneWayOnDemandStaticOffer" else if rideType == "Rental" then rideType<>"_OnDemandStaticOffer" else ""
        (scheduledBookingListResponse) <- Remote.rideBooking "10" (show state.data.offset) (state.data.date) (state.data.date) (tripCategory) ( fromMaybe "0.0" state.data.driverLat) ( fromMaybe "0.0" state.data.driverLong)
        case scheduledBookingListResponse of
          Right (ScheduledBookingListResponse listResp) -> do
            doAff do liftEffect $ push $ PastRideApiAC (ScheduledBookingListResponse listResp) "success"
            pure unit
          Left (err) -> do
            let errResp = err.response
                codeMessage = decodeErrorCode errResp.errorMessage
                noLocationError = err.code == 400 && codeMessage == "LOCATION_NOT_FOUND"
            if noLocationError then doAff do liftEffect $ push UpdateNoLocationFlag
              else doAff do liftEffect $ push $ PastRideApiAC (ScheduledBookingListResponse dummyListResp) if err.code == 500 then "listCompleted" else "failure"
            pure unit

dummyListResp :: forall a. { bookings :: Array a }
dummyListResp = { bookings: [] }


shimmerData:: Int -> ST.RideCardItemState
shimmerData i =   {
    date : toPropValue "Today - 13 Apr,2024",
    time : toPropValue "7 pm",
    source : toPropValue "Koramangla",
    distance : toPropValue "120",
    destination : toPropValue "Koramangla",
    totalAmount : toPropValue "2000",
    cardVisibility : toPropValue "visible",
    shimmerVisibility : toPropValue "",
    carImage : toPropValue "",
    rideType : toPropValue "Regular",
    vehicleType : toPropValue "Non-AC-Mini",
    srcLat :  toPropValue 0.0000,
    srcLong :  toPropValue 0.0000,
    desLat :  toPropValue 0.0000,
    desLong : toPropValue  0.0000,
    id : toPropValue "",
    image : toPropValue "",
    visible : toPropValue true,
    pillColor : toPropValue "",
    overlayVisiblity :toPropValue  "gone",
    visiblePill : toPropValue "" ,
    cornerRadius : toPropValue "",
    imageType : toPropValue "",
    estimatedDuration : toPropValue ""
}

getRideType :: RideRequestScreenState -> String
getRideType state = do
  let activeRideIndex =  state.data.activeRideIndex
  case state.data.pillViewArray !! activeRideIndex of
      Just pill -> maybe "" show pill.rideType
      Nothing -> ""
